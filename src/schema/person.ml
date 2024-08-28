(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

type role = Author | Editor
let role_to_string = function Author -> "author" | Editor -> "editor"
let pp_role ppf r = Fmt.string ppf (role_to_string r)

let role_type =
  let enc = function Author -> 0 | Editor -> 1 in
  let dec = function
  | 0 -> Author | 1 -> Editor | n -> Fmt.failwith "%d: Unknown role" n
  in
  Type.coded @@
  Type.Coded.make ~name:"Contributor.role" Type.int ~enc ~dec ~pp:pp_role

module Person = struct
  type id = Id.t
  type t =
    { id : id;
      last_name : string;
      first_names : string;
      orcid : string;
      note : string;
      private_note : string;
      public : bool; }

  let v ~id ~last_name ~first_names ~orcid ~note ~private_note ~public () =
    { id; last_name; first_names; orcid; note; private_note; public }

  let row id last_name first_names orcid note private_note public =
    { id; last_name; first_names; orcid; note; private_note; public }

  let new' =
    { id = 0; last_name = Uimsg.unnamed; first_names = ""; orcid = "";
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
    (p0.orcid <> "" && p0.orcid = p1.orcid) ||
    (p0.last_name = p1.last_name &&
     p0.first_names = p1.first_names)

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

  let id' = Col.make "id" Type.int id
  let last_name' = Col.make "last_name" Type.text last_name
  let first_names' = Col.make "first_names" Type.text first_names
  let orcid' = Col.make "orcid" Type.text orcid
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
type person = t

module Label = Label.For_entity (Person)

(* Queries *)

include Entity.Publicable_queries (Person)

open Rel_query.Syntax

let select sel =
  let* p = Bag.table table in
  let sel_by_id = Text.(of_int (p #. id') = sel) in
  let sel_by_last = Text.like (p #. last_name') Text.(sel ^ v "%") in
  let sel_by_first = Text.like (p #. first_names') Text.(sel ^ v "%") in
  Bag.where (sel_by_id || sel_by_last || sel_by_first) (Bag.yield p)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

let match' ~last ~first ~orcid =
  let* p = Bag.table table in
  let match_orcid = Text.(not (orcid = empty) && orcid = p #. orcid') in
  let match_lf = Text.(like (p #. last_name') last) &&
                 Text.(like (p #. first_names') first)
  in
  Bag.where (match_orcid || match_lf) (Bag.yield p)

let match_stmt =
  Rel_query.Sql.(func @@ text @-> text @-> text @-> ret (Table.row table)
                           (fun last first orcid -> match' ~last ~first ~orcid))

let match_stmt ~last ~first ~orcid = match_stmt last first orcid

(* URL requests *)

module Url = struct
  open Result.Syntax

  let role = "role"
  let role_of_query q = match Http.Query.find_first role q with
  | None -> Ok None
  | Some "editor" -> Ok (Some Editor)
  | Some "author" -> Ok (Some Author)
  | Some r ->
      let reason = Fmt.str "key %s: unknown role '%S'" role r in
      Http.Response.bad_request_400 ~reason ()

  let role_to_query ?(init = Http.Query.empty) = function
  | None -> init
  | Some r -> init |> Http.Query.def role (role_to_string r)

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
              Ok (Person.v ~id:0 ~last_name:l ~first_names:f ~orcid
                    ~note:"" ~private_note:"" ~public:true ())

  let person_to_query ?(init = Http.Query.empty) p =
    init
    |> Http.Query.def last (Person.last_name p)
    |> Http.Query.def first (Person.first_names p)
    |> Http.Query.def orcid (Person.orcid p)

  type named_id = string option * id
  type t =
  | Confirm_delete of id
  | Create
  | Delete of id
  | Duplicate of id
  | Duplicate_form of id
  | Edit_form of id
  | Index
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Input of
      Entity.Url.for_list * Entity.Url.input_name * role option * id
  | Input_create of
      Entity.Url.for_list * Entity.Url.input_name * role option * person
  | Input_finder of
      Entity.Url.for_list * Entity.Url.input_name * role option
  | Input_finder_find of
      Entity.Url.for_list * Entity.Url.input_name * role option * string
  | Update of id
  | View_fields of id

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["part"; "confirm-delete"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Confirm_delete id)
  | ["part"; "edit-form"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Edit_form id)
  | ["part"; "view-fields"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (View_fields id)
  | ["part"; "replace-form"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Replace_form id)
  | ["part"; "duplicate-form"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Duplicate_form id)
  | ["part"; "new-form"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let cancel = Entity.Url.cancel_url_of_query (Kurl.Bare.query u) in
      Kurl.ok (New_form { cancel })
  | ["part"; "input"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
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
      let* `POST, id = Entity.Url.meth_id u Http.Method.[post] id in
      Kurl.ok (Duplicate id)
  | ["action"; "replace"; id] ->
      let* `POST, id = Entity.Url.meth_id u Http.Method.[post] id in
      Kurl.ok (Replace id)
  | [name; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Page (Some name, id))
  | [id] ->
      let* meth, id = Entity.Url.meth_id u Http.Method.[get; put; delete] id in
      let url = match meth with
      | `GET -> Page (None, id) | `PUT -> Update id | `DELETE -> Delete id
      in
      Kurl.ok url
  | _ ->
      Kurl.no_match

  let html = ".html"
  let enc = function
  | Confirm_delete id ->
      Kurl.bare `GET ["part"; "confirm-delete"; Res.Id.to_string id]
  | Create ->
      Kurl.bare `POST [""]
  | Delete id ->
      Kurl.bare `DELETE [Res.Id.to_string id]
  | Duplicate id ->
      Kurl.bare `POST ["action"; "duplicate"; Res.Id.to_string id]
  | Duplicate_form id ->
      Kurl.bare `GET ["part"; "duplicate-form"; Res.Id.to_string id]
  | Edit_form id ->
      Kurl.bare `GET ["part"; "edit-form"; Res.Id.to_string id]
  | Index ->
      Kurl.bare `GET [""] ~ext:html
  | New_form { cancel } ->
      let query = Entity.Url.cancel_url_to_query cancel in
      Kurl.bare `GET ["part"; "new-form"] ?query
  | Page (None, id) ->
      Kurl.bare `GET [Res.Id.to_string id] ~ext:html
  | Page (Some n, id) ->
      Kurl.bare `GET [n; Res.Id.to_string id] ~ext:html
  | Replace id ->
      Kurl.bare `POST ["action"; "replace"; Res.Id.to_string id]
  | Replace_form id ->
      Kurl.bare `GET ["part"; "replace-form"; Res.Id.to_string id]
  | Input (for_list, n, role, id) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      let query = role_to_query ~init:query role in
      Kurl.bare `GET ["part"; "input"; Res.Id.to_string id] ~query
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
      Kurl.bare `PUT [Res.Id.to_string id]
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Res.Id.to_string id]

  let kind = Kurl.kind ~name:"person" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name p = Res.Named.name_of_string (names_fl p)
  let page p = Kurl.v kind (Page (Some (res_name p), id p))
end
