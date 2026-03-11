(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module Subject = struct
  module Id = Rel_kit.Id.MakeInt ()
  type t =
    { id : Id.t;
      name : string;
      parent : Id.t option;
      see : Id.t option;
      description : string;
      private_note : string;
      public : bool; }

  let make ~id ~name ~parent ~see ~description ~private_note ~public () =
    { id; name; parent; see; description; private_note; public }

  let row id name parent see description private_note public =
    { id; name; parent; see; description; private_note; public }

  let new' =
    { id = Id.zero; name = Uimsg.unnamed; parent = None; see = None;
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

  let id' = Col.make "id" Id.type' id
  let name' = Col.make "name" Type.text name
  let description' = Col.make "description" Type.text description
  let private_note' = Col.make "private_note" Type.text private_note
  let public' = Col.make "public" Type.bool public
  let parent' = Col.make "parent" Type.(option Id.type') parent
  let see' = Col.make "see" Type.(option Id.type') see
  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    let foreign_keys =
      [ Table.Foreign_key.make
          ~cols:[Def see']
          ~parent:(Self [Def id'])
          ~on_delete:`Cascade () (* FIXME maybe not *);
        Table.Foreign_key.make
          ~cols:[Def parent']
          ~parent:(Self [Def id'])
          ~on_delete:`Set_null ()]
    in
    Table.make "subject" ~primary_key ~foreign_keys @@
    Row.(unit row * id' * name' * parent' * see' * description' *
         private_note' * public');
end

include Subject
type subject = t

module See_also = struct
  type t = { given : Id.t; that : Id.t; }
  let make ~given ~that () = { given; that }
  let row given that = { given; that }
  let given s = s.given
  let that s = s.that
  let given' = Col.make "given" Id.type' given
  let that' = Col.make "that" Id.type' that
  let table =
    let primary_key = Table.Primary_key.make [Def given'; Def that'] in
    let foreign_keys =
      let parent = Table.Foreign_key.Table (table, [Col.Def id']) in
      let on_delete = `Cascade in
      [ Table.Foreign_key.make ~cols:[Def given'] ~parent ~on_delete ();
        Table.Foreign_key.make ~cols:[Def that'] ~parent ~on_delete (); ]
    in
    Table.make "subject_see_also" ~primary_key ~foreign_keys @@
    Row.(unit row * given' * that')

  let create r = Rel_sql.insert_into Db.dialect table r
end

module Label = Label.For_entity (Subject)

(* Queries *)

include Entity.Publicable_queries (Subject)

open Rel_query.Syntax

let some_id_is_public id =
  let eq_id id s = Id.(Option.get id = s #. id') in
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
  let publishable = Col.make "visible" Type.bool snd in
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
  let is_parent = Option.has_value ~eq:Id.equal id (s #. parent') in
  Bag.where is_parent (Bag.yield s)

let children_stmt =
  Rel_query.Sql.(func @@ Id.type' @-> ret (Table.row table) children)

let select sel =
  (* FIXME trim spaces in both pattern and scrutinee
     TODO the structure of this query is likely wrong [select]
     should have two args, the binding should convert to string. *)
  let* s = Bag.table table in
  let sel_by_id = Text.(Id.to_text (s #. id') = sel) in
  let sel_by_name = Text.(like (s #. name') (sel ^ v "%") ) in
  Bag.where (sel_by_id || sel_by_name) (Bag.yield s)

let select_stmt =
  Rel_query.Sql.(func @@ text @-> ret (Table.row table) select)

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Db.fold db st add Id.Map.empty


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
  | Input of Entity.Url.for_list * Entity.Url.input_name * Id.t
  | Input_create of Entity.Url.for_list * Entity.Url.input_name * subject
  | Input_finder of Entity.Url.for_list * Entity.Url.input_name
  | Input_finder_find of Entity.Url.for_list * Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of Id.t
  | Replace_form of Id.t
  | Update of Id.t
  | View_fields of Id.t

  let nameq = "name"
  let subject_of_query q = match Http.Query.find_first nameq q with
  | None -> Http.Response.bad_request_400 ~reason:"No subject found" ()
  | Some name ->
      Result.ok @@
      Subject.make ~id:Id.zero ~name ~parent:None ~see:None
        ~description:"" ~private_note:"" ~public:false ()

  let subject_to_query ?(init = Http.Query.empty) s =
    init |> Http.Query.def nameq (Subject.name s)

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
      Kurl.ok (Input (for_list, input_name, id))
  | ["part"; "input-create"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let* s = subject_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_create (for_list, input_name, s))
  | ["part"; "input-finder"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder (for_list, input_name))
  | ["part"; "input-finder-find"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* for_list = Entity.Url.for_list_of_query (Kurl.Bare.query u) in
      let* input_name = Entity.Url.input_name_of_query (Kurl.Bare.query u) in
      let q = Entity.Url.select_of_query (Kurl.Bare.query u) in
      Kurl.ok (Input_finder_find (for_list, input_name, q))
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
  | Input (for_list, n, id) ->
      let query = Http.Query.empty in
      let query = Entity.Url.for_list_to_query ~init:query for_list in
      let query = Entity.Url.input_name_to_query ~init:query n in
      Kurl.bare `GET ["part"; "input"; Id.to_string id] ~query
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
      Kurl.bare `PUT [Id.to_string id]
  | View_fields id  ->
      Kurl.bare `GET ["part"; "view-fields"; Id.to_string id]

  let kind = Kurl.kind ~name:"subject" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name s = Res.Named.name_of_string (name s)
  let page s = Kurl.v kind (Page (Some (res_name s), id s))
end
