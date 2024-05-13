(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module Container = struct
  type id = int
  type t =
    { id : id;
      title : string;
      isbn : string;
      issn : string;
      note : string;
      private_note : string;
      public : bool; }

  let v ~id ~title ~isbn ~issn ~note ~private_note ~public () =
    { id; title; note; isbn; issn; private_note; public }

  let row id title isbn issn note private_note public =
    { id; title; note; isbn; issn; private_note; public }

  let new' =
    { id = 0; title = Uimsg.untitled; isbn = ""; issn = "";
      note = ""; private_note = ""; public = false }

  let id c = c.id
  let title c = c.title
  let isbn c = c.isbn
  let issn c = c.issn
  let note c = c.note
  let private_note c = c.private_note
  let public c = c.public

  (* Operations *)

  let duplicate_data c =
    let title = String.concat " " [c.title; Uimsg.copy] in
    let public = false in
    { c with title; public }

  (* Predicates and comparisons *)

  let order_by_title c0 c1 = String.compare c0.title c1.title

  (* Derived data *)

  let index_letter c = (* Unicode :-( *)
    let title = title  c in
    let max = String.length title - 1 in
    let rec loop i =
      if i > max then None else match Char.lowercase_ascii title.[i] with
      | 'a' .. 'z' as c -> Some c
      | _ -> loop (i + 1)
    in
    loop 0

  (* Table *)

  let id' = Col.v "id" Type.Int id
  let title' = Col.v "title" Type.Text title
  let isbn' = Col.v "isbn" Type.Text isbn
  let issn' = Col.v "issn" Type.Text issn
  let note' = Col.v "note" Type.Text note
  let private_note' = Col.v "private_note" Type.Text private_note
  let public' = Col.v "public" Type.Bool public
  let table =
    let primary_key = [Col.V id'] in
    let row = Row.(unit row * id' * title' * isbn' * issn' * note' *
                   private_note' * public')
    in
    Table.v "container" row ~primary_key
end

include Container
type container = t
module Label = Label.For_entity (Container)

let match' ~title ~isbn ~issn =
  let open Rel_query.Syntax in
  let* c = Bag.table table in
  let match_isbn = Text.(not (isbn = empty) && isbn = c #. isbn') in
  let match_issn = Text.(not (issn = empty) && issn = c #. issn') in
  let match_title = Text.(like (c #. title') title) in
  Bag.where (match_isbn || match_issn || match_title) (Bag.yield c)

let match_stmt =
  Rel_query.Sql.(func @@ text @-> text @-> text @-> ret (Table.row table)
                           (fun title isbn issn -> match' ~title ~isbn ~issn))

let match_stmt ~title ~isbn ~issn = match_stmt title isbn issn

let select sel =
  (* FIXME trim spaces in both pattern and scrutinee *)
  let open Rel_query.Syntax in
  let* c = Bag.table table in
  let sel_by_id = Text.(of_int (c #. id') = sel) in
  let sel_by_title = Text.(like (c #. title') (sel ^ v "%") ) in
  Bag.where (sel_by_id || sel_by_title) (Bag.yield c)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

(* Queries *)

include Entity.Publicable_queries (Container)

(* URL requests *)

module Url = struct
  open Result.Syntax

  type named_id = string option * id
  type t =
  | Confirm_delete of id
  | Create
  | Delete of id
  | Duplicate of id
  | Duplicate_form of id
  | Edit_form of id
  | Index
  | Input of Entity.Url.input_name * id
  | Input_create of Entity.Url.input_name * container
  | Input_finder of Entity.Url.input_name
  | Input_finder_find of Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Update of id
  | View_fields of id (** *)

  let titleq = "title"
  let container_of_query q = match Http.Query.find_first titleq q with
  | None -> Http.Response.bad_request_400 ~reason:"No container found" ()
  | Some title ->
      Result.ok @@
      Container.v
        ~id:0 ~title ~isbn:"" ~issn:"" ~note:"" ~private_note:""
        ~public:false ()

  let container_to_query ?(init = Http.Query.empty) c =
    init |> Http.Query.def titleq (Container.title c)

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
      let* n = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input (n, id))
  | ["part"; "input-create"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* n = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* c = container_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_create (n, c))
  | ["part"; "input-finder"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* n = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder n)
  | ["part"; "input-finder-find"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* n = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let q = Entity.Url.select_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder_find (n, q))
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
  | Input (n, id) ->
      let query = Entity.Url.input_name_to_query n in
      Kurl.bare `GET ["part"; "input"; Res.Id.to_string id] ~query
  | Input_create (n, c) ->
      let query = container_to_query c in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input-create"] ~query
  | Input_finder n ->
      let query = Entity.Url.input_name_to_query n in
      Kurl.bare `GET ["part"; "input-finder"] ~query
  | Input_finder_find (n, sel) ->
      let query = Entity.Url.select_to_query sel in
      let query = Entity.Url.input_name_to_query ?init:query n in
      Kurl.bare `GET ["part"; "input-finder-find"] ~query
  | Update id ->
      Kurl.bare `PUT [Res.Id.to_string id]
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Res.Id.to_string id]

  let kind = Kurl.kind ~name:"container" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name c = Res.Named.name_of_string (title c)
  let page c = Kurl.v kind (Page (Some (res_name c), id c))
end
