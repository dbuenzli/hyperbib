(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module Container = struct
  module Id = Rel_kit.Id.MakeInt ()
  type t =
    { id : Id.t;
      title : string;
      isbn : string;
      issn : string;
      note : string;
      private_note : string;
      public : bool; }

  let make ~id ~title ~isbn ~issn ~note ~private_note ~public () =
    { id; title; note; isbn; issn; private_note; public }

  let row id title isbn issn note private_note public =
    { id; title; note; isbn; issn; private_note; public }

  let new' =
    { id = Id.zero; title = Uimsg.untitled; isbn = ""; issn = "";
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

  let id' = Col.make "id" Id.type' id
  let title' = Col.make "title" Type.text title
  let isbn' = Col.make "isbn" Type.text isbn
  let issn' = Col.make "issn" Type.text issn
  let note' = Col.make "note" Type.text note
  let private_note' = Col.make "private_note" Type.text private_note
  let public' = Col.make "public" Type.bool public
  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    Table.make "container" ~primary_key @@
    Row.(unit row * id' * title' * isbn' * issn' * note' * private_note' *
         public')
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

let select sel = (* TODO This query looks wrong. It's the reason why
                    we have Id.to_text. We should likely have two
                    different sel. If we want to use a single [sel]
                    it should be converted at the statement binding
                    level. *)
  (* FIXME trim spaces in both pattern and scrutinee *)
  let open Rel_query.Syntax in
  let* c = Bag.table table in
  let sel_by_id = Text.(Id.to_text (c #. id') = sel) in
  let sel_by_title = Text.(like (c #. title') (sel ^ v "%") ) in
  Bag.where (sel_by_id || sel_by_title) (Bag.yield c)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Db.fold db st add Id.Map.empty

(* Queries *)

include Entity.Publicable_queries (Container)

(* URL requests *)

module Url = struct
  open Result.Syntax

  type named_id = string option * Id.t
  type t =
  | Confirm_delete of Id.t
  | Create
  | Delete of Id.t
  | Duplicate of Id.t
  | Duplicate_form of Id.t
  | Edit_form of Id.t
  | Index
  | Input of Entity.Url.input_name * Id.t
  | Input_create of Entity.Url.input_name * container
  | Input_finder of Entity.Url.input_name
  | Input_finder_find of Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of Id.t
  | Replace_form of Id.t
  | Update of Id.t
  | View_fields of Id.t (** *)

  let titleq = "title"
  let container_of_query q = match Http.Query.find_first titleq q with
  | None -> Http.Response.bad_request_400 ~reason:"No container found" ()
  | Some title ->
      Result.ok @@
      Container.make
        ~id:Id.zero ~title ~isbn:"" ~issn:"" ~note:"" ~private_note:""
        ~public:false ()

  let container_to_query ?(init = Http.Query.empty) c =
    init |> Http.Query.def titleq (Container.title c)

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
  | Input (n, id) ->
      let query = Entity.Url.input_name_to_query n in
      Kurl.bare `GET ["part"; "input"; Id.to_string id] ~query
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
      Kurl.bare `PUT [Id.to_string id]
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Id.to_string id]

  let kind = Kurl.kind ~name:"container" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name c = Res.Named.name_of_string (title c)
  let page c = Kurl.v kind (Page (Some (res_name c), id c))
end
