(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

module Subject = struct
  type id = Id.t
  type t =
    { id : id;
      name : string;
      parent : id option;
      see : id option;
      description : string;
      private_note : string;
      public : bool; }

  let v ~id ~name ~parent ~see ~description ~private_note ~public () =
    { id; name; parent; see; description; private_note; public }

  let row id name parent see description private_note public =
    { id; name; parent; see; description; private_note; public }

  let new' =
    { id = 0; name = Uimsg.unnamed; parent = None; see = None;
      description = ""; private_note = ""; public = false }

  let id s = s.id
  let name s = s.name
  let parent s = s.parent
  let see s = s.see
  let description s = s.description
  let private_note s = s.private_note
  let public s = s.public

  (* Operations *)

  let duplicate_data s =
    let name = String.concat " " [s.name; Uimsg.copy] in
    let public = false in
    { s with name; public }

  (* Predicates and comparisons. *)

  let is_root s = Option.is_none s.parent
  let order_by_name s0 s1 = (* Unicode :-( *) compare s0.name s1.name

  (* Derived data *)

  let hierarchy ss =
    let add (roots, children) s = match parent s with
    | None -> (s :: roots), children
    | Some pid -> roots, (Id.Map.add_to_list pid s children)
    in
    List.fold_left add ([], Id.Map.empty) ss

  (* Table *)

  let id' = Col.v "id" Type.Int id
  let name' = Col.v "name" Type.Text name
  let description' = Col.v "description" Type.Text description
  let private_note' = Col.v "private_note" Type.Text private_note
  let public' = Col.v "public" Type.Bool public
  let parent' = Col.v "parent" Type.(Option Int) parent
  let see' = Col.v "see" Type.(Option Int) see
  let table =
    let row =
      Row.(unit row * id' * name' * parent' * see' * description' *
           private_note' * public');
    in
    let foreign_keys =
      [ Table.self_foreign_key ~cols:[Col.V see'] ~parent:[Col.V id']
          ~on_delete:`Cascade () (* FIXME maybe not *);
        Table.self_foreign_key ~cols:[Col.V parent'] ~parent:[Col.V id']
          ~on_delete:`Set_null () ]
    in
    Table.v "subject" row ~primary_key:[Col.V id'] ~foreign_keys
end

include Subject
type subject = t

module See_also = struct
  type t = { given : id; that : id; }
  let v ~given ~that () = { given; that }
  let row given that = { given; that }
  let given s = s.given
  let that s = s.that
  let given' = Col.v "given" Type.Int given
  let that' = Col.v "that" Type.Int that
  let table =
    let primary_key = Col.[V given'; V that'] in
    let parent = (table, [Col.V id']) and on_delete = `Cascade in
    let foreign_keys =
      [ Table.foreign_key ~cols:[Col.V given'] ~parent ~on_delete ();
        Table.foreign_key ~cols:[Col.V that'] ~parent ~on_delete (); ]
    in
    Table.v "subject_see_also" Row.(unit row * given' * that')
      ~primary_key ~foreign_keys

  let create r = Sql.insert_into Db.dialect table r
end

module Label = Label.For_entity (Subject)

(* Queries *)

include Entity.Publicable_queries (Subject)

open Rel_query.Syntax

let some_id_is_public id =
  let eq_id id s = Int.(Option.get id = s #. id') in
  Bag.exists @@
  let* s = Bag.table table in
  let sat = Option.is_none id || (eq_id id s && s #. public') in
  Bag.where sat (Bag.yield s)

let visible s =
  Bool.(s #. public' &&
        some_id_is_public (s #. see') &&
        some_id_is_public (s #. parent'))

let visible_list =
  let* s = Bag.table table in
  Bag.where (visible s) (Bag.yield s)

let visible_list_stmt = Rel_query.Sql.of_bag' table visible_list

let list_visibility =
  let* s = Bag.table table in
  Bag.yield (Bag.row (fun s p -> s, p) $ s $ visible s)

let list_visibility_stmt =
  let row = Table.row table in
  let publishable = Col.v "visible" Type.Bool snd in
  let ret = Row.unit (fun s p -> s, p) in
  let res = Row.(cat ret ~proj:fst row * publishable) in
  Rel_query.Sql.(func @@ ret res list_visibility)

let parents =
  let* s = Bag.table table in
  let has_no_parent = Option.is_none (s #. parent') in
  Bag.where has_no_parent (Bag.yield s)

let parents_stmt = Rel_query.Sql.of_bag' table parents

let children id =
  let* s = Bag.table table in
  let is_parent = Option.has_value ~eq:Int.equal id (s #. parent') in
  Bag.where is_parent (Bag.yield s)

let children_stmt =
  Rel_query.Sql.(func @@ int @-> ret (Table.row table) children)

let select sel =
  (* FIXME trim spaces in both pattern and scrutinee *)
  let* s = Bag.table table in
  let sel_by_id = Text.(of_int (s #. id') = sel) in
  let sel_by_name = Text.(like (s #. name') (sel ^ v "%") ) in
  Bag.where (sel_by_id || sel_by_name) (Bag.yield s)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

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
  | Input of Entity.Url.for_list * Entity.Url.input_name * id
  | Input_create of Entity.Url.for_list * Entity.Url.input_name * subject
  | Input_finder of Entity.Url.for_list * Entity.Url.input_name
  | Input_finder_find of Entity.Url.for_list * Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Update of id
  | View_fields of id

  let nameq = "name"
  let subject_of_query q = match Http.Query.find nameq q with
  | None -> Http.Resp.bad_request_400 ~reason:"No subject found" ()
  | Some name ->
      Result.ok @@
      Subject.v ~id:0 ~name ~parent:None ~see:None
        ~description:"" ~private_note:"" ~public:false ()

  let subject_to_query ?(init = Http.Query.empty) s =
    Http.Query.add nameq (Subject.name s) init

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Meth.[get; post] u in
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
      let* `GET = Kurl.allow Http.Meth.[get] u in
      let cancel = Entity.Url.cancel_url_of_query (Kurl.Bare.query u) in
      Kurl.ok (New_form { cancel })
  | ["part"; "input"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input (for_list, input_name, id))
  | ["part"; "input-create"] ->
      let* `GET = Kurl.allow Http.Meth.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* s = subject_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_create (for_list, input_name, s))
  | ["part"; "input-finder"] ->
      let* `GET = Kurl.allow Http.Meth.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder (for_list, input_name))
  | ["part"; "input-finder-find"] ->
      let* `GET = Kurl.allow Http.Meth.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let q = Entity.Url.select_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder_find (for_list, input_name, q))
  | ["action"; "duplicate"; id] ->
      let* `POST, id = Entity.Url.meth_id u Http.Meth.[post] id in
      Kurl.ok (Duplicate id)
  | ["action"; "replace"; id] ->
      let* `POST, id = Entity.Url.meth_id u Http.Meth.[post] id in
      Kurl.ok (Replace id)
  | [name; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Page (Some name, id))
  | [id] ->
      let* meth, id = Entity.Url.meth_id u Http.Meth.[get; put; delete] id in
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
  | Input (for_list, n, id) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input"; Res.Id.to_string id] ~query
  | Input_create (for_list, n, s) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = subject_to_query ~init:query s in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input-create"] ~query
  | Input_finder (for_list, n) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input-finder"] ~query
  | Input_finder_find (for_list, n, sel) ->
      let query = Entity.Url.select_to_query sel in
      let query = Entity.Url.for_list_to_query ?init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input-finder-find"] ~query
  | Update id ->
      Kurl.bare `PUT [Res.Id.to_string id]
  | View_fields id  ->
      Kurl.bare `GET ["part"; "view-fields"; Res.Id.to_string id]

  let kind = Kurl.kind ~name:"subject" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name s = Res.Named.name_of_string (name s)
  let page s = Kurl.v kind (Page (Some (res_name s), id s))
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
