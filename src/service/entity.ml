(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

module type IDENTIFIABLE = sig
  type id = Id.t
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
  type id = Id.t
  type t
  val create : ignore_id:bool -> t -> unit Sql.Stmt.t
  val create_cols : ignore_id:bool -> t Col.value list -> unit Sql.Stmt.t
  val delete : id -> unit Sql.Stmt.t
  val update : id -> t Col.value list -> unit Sql.Stmt.t
  val find_id : id Ask.value -> (t, Bag.unordered) Bag.t
  val find_id_stmt : id -> t Sql.Stmt.t
  val find_ids : (id, 'a) Bag.t -> (t, Bag.unordered) Bag.t
end

module type IDENTIFIABLE_WITH_QUERIES = sig
  include IDENTIFIABLE
  include IDENTIFIABLE_QUERIES with type t := t and type id = id
end

module Identifiable_queries (E : IDENTIFIABLE) :
  (IDENTIFIABLE_QUERIES with type t := E.t and type id := E.id) = struct

  open Ask.Syntax

  type id = E.id
  type t = E.t

  let create ~ignore_id p =
    let ignore = if ignore_id then [Col.V E.id'] else [] in
    Sql.insert_into ~ignore E.table p

  let create_cols ~ignore_id cols =
    let ignore = if ignore_id then [Col.V E.id'] else [] in
    Sql.insert_into_cols ~ignore E.table cols

  let delete id =
    Sql.delete_from E.table ~where:[Col.Value (E.id', id)]

  let find_id id =
    let* p = Bag.table E.table in
    let eq_id = Int.(p #. E.id' = id) in
    Bag.where eq_id (Bag.yield p)

  let find_id_stmt =
    Sql.Bag.(func @@ int @-> ret (Table.row E.table) find_id)

  let find_ids ids = let* id = ids in find_id id

  let update id cols =
    Sql.update E.table ~set:cols ~where:[Col.Value (E.id', id)]
end


module type PUBLICABLE_QUERIES = sig
  include IDENTIFIABLE_QUERIES
  val list : only_public:bool Ask.value -> (t, Ask.Bag.unordered) Ask.Bag.t
  val list_stmt : only_public:bool -> t Sql.Stmt.t
end

module type PUBLICABLE_WITH_QUERIES = sig
  include PUBLICABLE
  include PUBLICABLE_QUERIES with type t := t and type id = id
end

module Publicable_queries (E : PUBLICABLE) :
  (PUBLICABLE_QUERIES with type t := E.t and type id := E.id) = struct

  include Identifiable_queries (E)

  open Ask.Syntax

  let list ~only_public =
    let* r = Bag.table E.table in
    let sat = Bool.(not only_public || r #. E.public') in
    Bag.where sat (Bag.yield r)

  let list_stmt =
    let f =
      Sql.Bag.(func @@ bool @-> ret (Table.row E.table) (fun b -> list b))
    in
    fun ~only_public -> f only_public
end

(* Entity URLs *)

module Url = struct
  open Result.Syntax

  let replace_by = "replace-by"
  let replace_by_of_query q = match Http.Query.find replace_by q with
  | None -> Resp.bad_request_400 ()
  | Some r -> Res.Id.decode r

  type cancel_url = string option
  let cancel = "cancel"
  let cancel_url_of_query query = Http.Query.find cancel query
  let cancel_url_to_query = function
  | None -> None | Some goto -> Some (Http.Query.(empty |> add cancel goto))

  let select = "select"
  let select_of_query q = Option.value ~default:"" (Http.Query.find select q)
  let select_to_query = function
  | "" -> None
  | sel -> Some (Http.Query.(empty |> add select sel))

  let meth_id u ms id =
    let* meth = Kurl.Allow.meths ms u in
    let* id = Res.Id.decode id in
    Ok (meth, id)

  let get_id u id = meth_id u Kurl.Allow.[get] id
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
