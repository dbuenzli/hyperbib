(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module type ID = sig
  type t

  module Rel : sig
    val type' : t Rel.Type.t
    val v : t -> t Rel_query.value
    val equal : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
    val ( = ) : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  end
end

module type IDENTIFIABLE = sig
  type id
  type t
  val id : t -> id
  val id' : (t, id) Col.t
  val table : t Table.t
end

module type PUBLICABLE = sig
  include IDENTIFIABLE
  val public : t -> bool
  val public' : (t, bool) Col.t
end

module type NAMED = sig
  include PUBLICABLE
  val name : t -> string
  val name' : (t, string) Col.t
end

module type ANNOTABLE = sig
  include PUBLICABLE
  val note : t -> string
  val note' : (t, string) Col.t
end

module type PRIVATELY_ANNOTABLE = sig
  include PUBLICABLE
  val private_note : t -> string
  val private_note' : (t, string) Col.t
end

module type DESCRIBABLE = sig
  include PUBLICABLE
  val description : t -> string
  val description' : (t, string) Col.t
end

module type IDENTIFIABLE_QUERIES = sig
  type id
  type t
  val create : ignore_id:bool -> t -> unit Rel_sql.Stmt.t
  val create_cols : ignore_id:bool -> t Col.value list -> unit Rel_sql.Stmt.t
  val delete : id -> unit Rel_sql.Stmt.t
  val update : id -> t Col.value list -> unit Rel_sql.Stmt.t
  val find_id : id Rel_query.value -> (t, Bag.unordered) Bag.t
  val find_id_stmt : id -> t Rel_sql.Stmt.t
  val find_ids : (id, 'a) Bag.t -> (t, Bag.unordered) Bag.t
  val find_id_list : id list -> (t, Bag.unordered) Bag.t
end

module type IDENTIFIABLE_WITH_QUERIES = sig
  include IDENTIFIABLE
  include IDENTIFIABLE_QUERIES with type t := t and type id := id
end

module Identifiable_queries (Id : ID) (E : IDENTIFIABLE with type id = Id.t) :
  (IDENTIFIABLE_QUERIES with type t := E.t and type id := E.id) = struct

  open Rel_query.Syntax

  type id = E.id
  type t = E.t

  let create ~ignore_id p =
    let ignore = if ignore_id then [Col.Def E.id'] else [] in
    Rel_sql.insert_into Db.dialect ~ignore E.table p

  let create_cols ~ignore_id cols =
    let ignore = if ignore_id then [Col.Def E.id'] else [] in
    Rel_sql.insert_into_cols Db.dialect ~ignore E.table cols

  let delete id =
    Rel_sql.delete_from Db.dialect E.table ~where:[Col.Value (E.id', id)]

  let find_id id =
    let* p = Bag.table E.table in
    let eq_id = Id.Rel.(p #. E.id' = id) in
    Bag.where eq_id (Bag.yield p)

  let find_id_stmt =
    Rel_query.Sql.(func @@ Id.Rel.type' @-> ret (Table.row E.table) find_id)

  let find_ids ids = let* id = ids in find_id id

  let find_id_list ids =
    let add acc id = Bag.union (find_id (Id.Rel.v id)) acc in
    match ids with
    | [] -> Bag.empty
    | id :: ids ->
        (* FIXME rel bug: we want to fold starting with Bag.empty *)
        List.fold_left add (find_id (Id.Rel.v id)) ids

  let update id cols =
    Rel_sql.update Db.dialect E.table ~set:cols ~where:[Col.Value (E.id', id)]
end


module type PUBLICABLE_QUERIES = sig
  include IDENTIFIABLE_QUERIES
  val list : only_public:bool Rel_query.value -> (t, Bag.unordered) Bag.t
  val list_stmt : only_public:bool -> t Rel_sql.Stmt.t
end

module type PUBLICABLE_WITH_QUERIES = sig
  include PUBLICABLE
  include PUBLICABLE_QUERIES with type t := t and type id := id
end

module Publicable_queries (Id : ID) (E : PUBLICABLE with type id = Id.t) :
  (PUBLICABLE_QUERIES with type t := E.t and type id := E.id) = struct

  include Identifiable_queries (Id) (E)

  open Rel_query.Syntax

  let list ~only_public =
    let* r = Bag.table E.table in
    let sat = Bool.(not only_public || r #. E.public') in
    Bag.where sat (Bag.yield r)

  let list_stmt =
    let f =
      Rel_query.Sql.(func @@ bool @-> ret (Table.row E.table)
                               (fun b -> list ~only_public:b))
    in
    fun ~only_public -> f only_public
end

(* Entity URLs *)

module Url = struct
  open Result.Syntax

  type query_key = string

  let replace_by = "replace-by"

  (* XXX remove eventually *)
  let replace_by_of_query q = match Http.Query.find_first replace_by q with
  | None -> Http.Response.bad_request_400 ()
  | Some r -> Res.Id.decode r

  let replace_by_of_query' q = match Http.Query.find_first replace_by q with
  | None | Some "" -> Ok None
  | Some r -> Result.map Option.some (Res.Id.decode r)


  type cancel_url = string option
  let cancel = "cancel"
  let cancel_url_of_query query = Http.Query.find_first cancel query
  let cancel_url_to_query = function
  | None -> None
  | Some goto -> Some (Http.Query.empty |> Http.Query.def cancel goto)

  let select = "select"
  let select_of_query q =
    Option.value ~default:"" (Http.Query.find_first select q)

  let select_to_query = function
  | "" -> None
  | sel -> Some (Http.Query.empty |> Http.Query.def select sel)

  type input_name = string
  let input_name = "input-name"
  let input_name_of_query q = match Http.Query.find_first input_name q with
  | None -> Http.Response.bad_request_400 ()
  | Some n -> Ok n

  type for_list = bool
  let for_list = "for-list"
  let for_list_of_query q = match Http.Query.find_first for_list q with
  | None -> Ok false
  | Some bool ->
      match bool_of_string_opt bool with
      | None -> Http.Response.bad_request_400 ~reason:"%S: not a boolean" ()
      | Some bool -> Ok bool

  let for_list_to_query ?(init = Http.Query.empty) b =
    init |> Http.Query.def for_list (string_of_bool b)


  let input_name_to_query ?(init = Http.Query.empty) n =
    init |> Http.Query.def input_name n

  let meth_id u ms id =
    let* meth = Kurl.allow ms u in
    let* id = Res.Id.decode id in
    Ok (meth, id)

  let get_id u id = meth_id u Http.Method.[get] id
end
