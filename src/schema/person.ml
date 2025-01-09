(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module Role = struct
  type t = Author | Editor
  let to_string = function Author -> "author" | Editor -> "editor"
  let pp ppf r = Fmt.string ppf (to_string r)

  let type' =
    let enc = function Author -> 0 | Editor -> 1 in
    let dec = function
    | 0 -> Author | 1 -> Editor | n -> Fmt.failwith "%d: Unknown role" n
    in
    Type.coded @@
    Type.Coded.make ~name:"Person.Role.t" Type.int ~enc ~dec ~pp
end

module Person = struct
  module Id = Rel_kit.Id.MakeInt ()
  type t =
    { id : Id.t;
      last_name : string;
      first_names : string;
      orcid : Orcid.t option;
      note : string;
      private_note : string;
      public : bool; }

  let make ~id ~last_name ~first_names ~orcid ~note ~private_note ~public () =
    { id; last_name; first_names; orcid; note; private_note; public }

  let row id last_name first_names orcid note private_note public =
    { id; last_name; first_names; orcid; note; private_note; public }

  let new' =
    { id = Id.zero; last_name = Uimsg.unnamed; first_names = ""; orcid = None;
      note = ""; private_note = ""; public = false }

  let id p = p.id
  let last_name p = p.last_name
  let first_names p = p.first_names
  let orcid p = p.orcid
  let note p = p.note
  let private_note p = p.private_note
  let public p = p.public

  (* Operations *)

  let duplicate_data p =
    let last_name = String.concat " " [p.last_name; Uimsg.copy] in
    let public = false in
    { p with last_name; public }

  let created_equal p0 p1 =
    let orcid_equal = match p1.orcid, p1.orcid with
    | Some id1, Some id2 when Orcid.equal id1 id2 -> true
    | _ -> false
    in
    (orcid_equal ||
    (p0.last_name = p1.last_name && p0.first_names = p1.first_names))

  (* Predicates and comparisons *)

  let order_by_last_name p0 p1 =
    let c = String.compare (last_name p0) (last_name p1) in
    if c <> 0 then c else
    String.compare (first_names p0) (first_names p1)

  (* Derived data *)

  let names_fl p = match first_names p, last_name p with
  | "", "" -> Uimsg.unknown
  | "", n | n, "" -> n
  | f, l -> String.concat " " [f; l]

  let names_lf p = match first_names p, last_name p with
  | "", "" -> Uimsg.unknown
  | "", n | n, "" -> n
  | f, l -> String.concat ", " [l; f]

  let index_letter p = (* Unicode :-( *)
    let last = last_name p in
    let max = String.length last - 1 in
    let rec loop i =
      if i > max then None else match Char.lowercase_ascii last.[i] with
      | 'a' .. 'z' as c -> Some c
      | _ -> loop (i + 1)
    in
    loop 0

  (* Table *)

  let id' = Col.make "id" Id.type' id
  let last_name' = Col.make "last_name" Type.text last_name
  let first_names' = Col.make "first_names" Type.text first_names
  let orcid' = Col.make "orcid" Type.(option Schema_kit.Orcid_rel.t) orcid
  let note' = Col.make "note" Type.text note
  let private_note' = Col.make "private_note" Type.text private_note
  let public' = Col.make "public" Type.bool public
  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    Table.make "person" ~primary_key @@
    Row.(unit row * id' * last_name' * first_names' * orcid' * note' *
         private_note' * public')
end

include Person

module Label = Label.For_entity (Person)

(* Queries *)

include Entity.Publicable_queries (Person)

open Rel_query.Syntax
open Schema_kit

let select sel =
  (* TODO the structure of this query is wrong. select should
     have two args. *)
  let* p = Bag.table table in
  let sel_by_id = Text.(Id.to_text (p #. id') = sel) in
  let sel_by_last = Text.like (p #. last_name') Text.(sel ^ v "%") in
  let sel_by_first = Text.like (p #. first_names') Text.(sel ^ v "%") in
  Bag.where (sel_by_id || sel_by_last || sel_by_first) (Bag.yield p)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

let match' ~last ~first ~orcid =
  let* p = Bag.table table in
  let match_orcid =
    Option.is_some orcid &&
    Option.equal ~eq:Orcid_rel.equal orcid (p #. orcid')
  in
  let match_lf = Text.(like (p #. last_name') last) &&
                 Text.(like (p #. first_names') first)
  in
  Bag.where (match_orcid || match_lf) (Bag.yield p)

let match_stmt =
  (* FIXME Rel the reverse of args is annoying here. *)
  let match' orcid first last = match' ~last ~first ~orcid in
  Rel_query.Sql.func @@
  Rel_query.Sql.(text @-> text @-> option Orcid_rel.t @->
                 ret (Table.row table) match')

let match_stmt ~last ~first ~orcid = match_stmt last first orcid

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Db.fold db st add Id.Map.empty

(* URL requests *)

module Url = struct
  open Result.Syntax
  type person = t

  let role = "role"
  let role_of_query q = match Http.Query.find_first role q with
  | None -> Ok None
  | Some "editor" -> Ok (Some Role.Editor)
  | Some "author" -> Ok (Some Role.Author)
  | Some r ->
      let reason = Fmt.str "key %s: unknown role '%S'" role r in
      Http.Response.bad_request_400 ~reason ()

  let role_to_query ?(init = Http.Query.empty) = function
  | None -> init
  | Some r -> init |> Http.Query.def role (Role.to_string r)

  let first = "first"
  let last = "last"
  let orcid = "orcid"
  let person_of_query q = match Http.Query.find_first first q with
  | None -> Http.Response.bad_request_400 ~reason:"No person found" ()
  | Some f ->
      match Http.Query.find_first last q with
      | None -> Http.Response.bad_request_400 ~reason:"No person found" ()
      | Some l ->
          match Http.Query.find_first orcid q with
          | None -> Http.Response.bad_request_400 ~reason:"No person found" ()
          | Some orcid ->
              let orcid =
                if orcid = "" then None else
                (* XXX form validation *)
                match Orcid.of_string orcid with
                | Ok v -> Some v | Error _ -> None
              in
              Ok (Person.make ~id:Id.zero ~last_name:l ~first_names:f ~orcid
                    ~note:"" ~private_note:"" ~public:true ())

  let person_to_query ?(init = Http.Query.empty) p =
    init
    |> Http.Query.def last (Person.last_name p)
    |> Http.Query.def first (Person.first_names p)
    |> Http.Query.def orcid
      (match Person.orcid p with None -> "" | Some o -> Orcid.to_string o)

  type named_id = string option * Id.t
  type t =
  | Confirm_delete of Id.t
  | Create
  | Delete of Id.t
  | Duplicate of Id.t
  | Duplicate_form of Id.t
  | Edit_form of Id.t
  | Index
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of Id.t
  | Replace_form of Id.t
  | Input of
      Entity.Url.for_list * Entity.Url.input_name * Role.t option * Id.t
  | Input_create of
      Entity.Url.for_list * Entity.Url.input_name * Role.t option * person
  | Input_finder of
      Entity.Url.for_list * Entity.Url.input_name * Role.t option
  | Input_finder_find of
      Entity.Url.for_list * Entity.Url.input_name * Role.t option * string
  | Update of Id.t
  | View_fields of Id.t

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["part"; "confirm-delete"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Confirm_delete id)
  | ["part"; "edit-form"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Edit_form id)
  | ["part"; "view-fields"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (View_fields id)
  | ["part"; "replace-form"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Replace_form id)
  | ["part"; "duplicate-form"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Duplicate_form id)
  | ["part"; "new-form"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let cancel = Entity.Url.cancel_url_of_query (Kurl.Bare.query u) in
      Kurl.ok (New_form { cancel })
  | ["part"; "input"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* role = role_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input (for_list, input_name, role, id))
  | ["part"; "input-create"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* role = role_of_query (Kurl.Bare.query u) in
      let* p = person_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_create (for_list, input_name, role, p))
  | ["part"; "input-finder"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* role = role_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder (for_list, input_name, role))
  | ["part"; "input-finder-find"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* role = role_of_query (Kurl.Bare.query u) in
      let q = Entity.Url.select_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder_find (for_list, input_name, role, q))
  | ["action"; "duplicate"; id] ->
      let* `POST, id = Entity.Url.meth_id (module Id) u Http.Method.[post] id in
      Kurl.ok (Duplicate id)
  | ["action"; "replace"; id] ->
      let* `POST, id = Entity.Url.meth_id (module Id) u Http.Method.[post] id in
      Kurl.ok (Replace id)
  | [name; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Page (Some name, id))
  | [id] ->
      let* meth, id =
        Entity.Url.meth_id (module Id) u Http.Method.[get; put; delete] id
      in
      let url = match meth with
      | `GET -> Page (None, id) | `PUT -> Update id | `DELETE -> Delete id
      in
      Kurl.ok url
  | _ ->
      Kurl.no_match

  let html = ".html"
  let enc = function
  | Confirm_delete id ->
      Kurl.bare `GET ["part"; "confirm-delete"; Id.to_string id]
  | Create ->
      Kurl.bare `POST [""]
  | Delete id ->
      Kurl.bare `DELETE [Id.to_string id]
  | Duplicate id ->
      Kurl.bare `POST ["action"; "duplicate"; Id.to_string id]
  | Duplicate_form id ->
      Kurl.bare `GET ["part"; "duplicate-form"; Id.to_string id]
  | Edit_form id ->
      Kurl.bare `GET ["part"; "edit-form"; Id.to_string id]
  | Index ->
      Kurl.bare `GET [""] ~ext:html
  | New_form { cancel } ->
      let query = Entity.Url.cancel_url_to_query cancel in
      Kurl.bare `GET ["part"; "new-form"] ?query
  | Page (None, id) ->
      Kurl.bare `GET [Id.to_string id] ~ext:html
  | Page (Some n, id) ->
      Kurl.bare `GET [n; Id.to_string id] ~ext:html
  | Replace id ->
      Kurl.bare `POST ["action"; "replace"; Id.to_string id]
  | Replace_form id ->
      Kurl.bare `GET ["part"; "replace-form"; Id.to_string id]
  | Input (for_list, n, role, id) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      let query = role_to_query ~init:query role in
      Kurl.bare `GET ["part"; "input"; Id.to_string id] ~query
  | Input_create (for_list, n, role, s) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = person_to_query ~init:query s in
      let query = Entity.Url.input_name_to_query ~init:query n in
      let query = role_to_query ~init:query role in
      Kurl.bare `GET ["part"; "input-create"] ~query
  | Input_finder (for_list, n, role) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      let query = role_to_query ~init:query role in
      Kurl.bare `GET ["part"; "input-finder"] ~query
  | Input_finder_find (for_list, n, role, sel) ->
      let query = Entity.Url.select_to_query sel in
      let query = Entity.Url.input_name_to_query ?init:query n in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = role_to_query ~init:query role in
      Kurl.bare `GET ["part"; "input-finder-find"] ~query
  | Update id ->
      Kurl.bare `PUT [Id.to_string id]
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Id.to_string id]

  let kind = Kurl.kind ~name:"person" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name p = Res.Named.name_of_string (names_fl p)
  let page p = Kurl.v kind (Page (Some (res_name p), id p))
end
