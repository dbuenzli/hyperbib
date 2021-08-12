(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

type role = Author | Editor
let role_to_string = function Author -> "author" | Editor -> "editor"
let pp_role ppf r = Fmt.string ppf (role_to_string r)

let role_enc = function Author -> Ok 0 | Editor -> Ok 1
let role_dec = function
| 0 -> Ok Author | 1 -> Ok Editor | n -> Fmt.error "%d: unknown role" n

let role_type =
  let c = Type.Coded.v ~name:"Contributor.role" role_enc role_dec Type.Int in
  Type.Coded c

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

  let id' = Col.v "id" Type.Int id
  let last_name' = Col.v "last_name" Type.Text last_name
  let first_names' = Col.v "first_names" Type.Text first_names
  let orcid' = Col.v "orcid" Type.Text orcid
  let note' = Col.v "note" Type.Text note
  let private_note' = Col.v "private_note" Type.Text private_note
  let public' = Col.v "public" Type.Bool public
  let table =
    Table.v "person"
      Row.(unit row * id' * last_name' * first_names' * orcid' * note' *
           private_note' * public')
      ~params:Table.[Primary_key [Col.V id']]
end

include Person
type person = t

module Label = Label.For_entity (Person)

(* Queries *)

include Entity.Publicable_queries (Person)

open Ask.Syntax

let select sel =
  let* p = Bag.table table in
  let sel_by_id = Text.(of_int (p #. id') = sel) in
  let sel_by_last = Text.like (p #. last_name') Text.(sel ^ v "%") in
  let sel_by_first = Text.like (p #. first_names') Text.(sel ^ v "%") in
  Bag.where (sel_by_id || sel_by_last || sel_by_first) (Bag.yield p)

let select_stmt =
  Sql.Bag.(func @@ text @-> ret (Table.row table) select)

let match' ~last ~first ~orcid =
  let* p = Bag.table table in
  let match_orcid = Text.(not (orcid = empty) && orcid = p #. orcid') in
  let match_lf = Text.(like (p #. last_name') last) &&
                 Text.(like (p #. first_names') first)
  in
  Bag.where (match_orcid || match_lf) (Bag.yield p)

let match_stmt =
  Sql.Bag.(func @@ text @-> text @-> text @-> ret (Table.row table)
                     (fun last first orcid -> match' ~last ~first ~orcid))

let match_stmt ~last ~first ~orcid = match_stmt last first orcid

(* URL requests *)

module Url = struct
  open Result.Syntax

  let role = "role"
  let role_of_query q = match Http.Query.find role q with
  | None -> Ok None
  | Some "editor" -> Ok (Some Editor)
  | Some "author" -> Ok (Some Author)
  | Some r ->
      let reason = Fmt.str "key %s: unknown role '%S'" role r in
      Resp.bad_request_400 ~reason ()

  let role_to_query ?(init = Http.Query.empty) = function
  | None -> init
  | Some r -> Http.Query.add role (role_to_string r) init

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
  | Select of role option * string
  | Select_add of role option * id
  | Update of id
  | View_fields of id

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.Allow.(meths [get; post] u) in
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
      let* `GET = Kurl.Allow.(meths [get] u) in
      let cancel = Entity.Url.cancel_url_of_query (Kurl.Bare.query u) in
      Kurl.ok (New_form { cancel })
  | ["part"; "select"] ->
      let* `GET = Kurl.Allow.(meths [get] u) in
      let q = Entity.Url.select_of_query (Kurl.Bare.query u) in
      let* r = role_of_query (Kurl.Bare.query u) in
      Kurl.ok (Select (r, q))
  | ["part"; "select"; "add"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      let* r = role_of_query (Kurl.Bare.query u) in
      Kurl.ok (Select_add (r, id))
  | ["action"; "duplicate"; id] ->
      let* `POST, id = Entity.Url.meth_id u Kurl.Allow.[post] id in
      Kurl.ok (Duplicate id)
  | ["action"; "replace"; id] ->
      let* `POST, id = Entity.Url.meth_id u Kurl.Allow.[post] id in
      Kurl.ok (Replace id)
  | [name; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Page (Some name, id))
  | [id] ->
      let* meth, id = Entity.Url.meth_id u Kurl.Allow.[get; put; delete] id in
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
  | Select (r, sel) ->
      (* FIXME composition *)
      let query =
        Stdlib.Option.value
          ~default:Http.Query.empty (Entity.Url.select_to_query sel)
      in
      let query = role_to_query ~init:query r in
      Kurl.bare `GET ["part"; "select"] ~query
  | Select_add (r, id) ->
      let query = role_to_query r in
      Kurl.bare `GET ["part"; "select"; "add"; Res.Id.to_string id] ~query
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

(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern

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
