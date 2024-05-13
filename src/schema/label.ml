(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

(* Labels *)

module Label = struct
  type id = Id.t
  type name = string
  type color = int64
  type t =
    { id : id;
      name : string;
      synopsis : string;
      color : color;
      note : string;
      private_note : string;
      public : bool; }

  type label = t

  let v ~id ~name ~synopsis ~color ~note ~private_note ~public =
    { id; name; synopsis; color; note; private_note; public }

  let row id name synopsis color note private_note public =
    { id; name; synopsis; color; note; private_note; public }

  let id l = l.id
  let name l = l.name
  let synopsis l = l.synopsis
  let color l = l.color
  let note l = l.note
  let private_note l = l.private_note
  let public l = l.public

  let order_by_name l0 l1 = String.compare l0.name l1.name

  (* Table *)

  let id' = Col.v "id" Type.Int id
  let name' = Col.v "name" Type.Text name
  let synopsis' = Col.v "synopsis" Type.Text synopsis
  let color' = Col.v "color" Type.Int64 color
  let note' = Col.v "note" Type.Text note
  let private_note' = Col.v "private_note" Type.Text private_note
  let public' = Col.v "public" Type.Bool public
  let table =
    let primary_key = [Col.V id'] in
    let unique_keys = [Table.unique_key [Col.V name']] in
    Table.v "label"
      Row.(unit row * id' * name' * synopsis' * color' * note' * private_note' *
           public') ~primary_key ~unique_keys
end

include Label

(* Labeling *)

type label = t
type label_id = id

module type APPLICATION = sig
  type entity
  type entity_id = Id.t
  type t
  val v : entity:entity_id -> label:id -> t
  val row : entity_id -> id -> t
  val entity : t -> entity_id
  val label : t -> id
  val entity' : (t, entity_id) Col.t
  val label' : (t, id) Col.t
  val table : t Table.t
  val create : t -> unit Rel_sql.Stmt.t
  val applications : (entity_id, 'a) Bag.t -> (t, Bag.unordered) Bag.t
  val of_applications :
    only_public:
      bool Rel_query.value -> (t, 'a) Bag.t -> (label, Bag.unordered) Bag.t
  val copy_applications_stmt :
    src:entity_id -> dst:entity_id -> unit Rel_sql.Stmt.t
end

module For_entity (E : Entity.IDENTIFIABLE) = struct
  type entity = E.t
  type entity_id = E.id
  type t = { entity : E.id; label : id; }

  let v ~entity ~label = { entity; label }
  let row entity label = { entity; label }
  let entity e = e.entity
  let label e = e.label

  let entity' = Col.v "entity" Type.Int entity
  let label' = Col.v "label" Type.Int label
  let table =
    let name = Table.name E.table ^ "_label" in
    let primary_key = [Col.V entity'; Col.V label'] in
    let foreign_keys = Table.[
        foreign_key ~cols:Col.[V entity'] ~parent:(E.table, Col.[V E.id'])
          ~on_delete:`Cascade ();
        foreign_key ~cols:Col.[V label'] ~parent:(Label.table, Col.[V id'])
          ~on_delete:`Cascade ()]
    in
    let indices = [Table.index [Col.V label']] in
    Table.v name Row.(unit row * entity' * label')
      ~primary_key ~foreign_keys ~indices

  open Rel_query.Syntax

  let create r = Rel_sql.insert_into Db.dialect table r

  let applications eids =
    let* eid = eids in
    let* rel = Bag.table table in
    Bag.where Int.(eid = rel #. entity') (Bag.yield rel)

  let of_applications ~only_public apps =
    let* label = Bag.table Label.table in
    let select =
      Bag.exists @@
      let* app = apps in
      let is_used = Int.(app #. label' = label #. id') in
      let filter = Bool.(not only_public || label #. public') in
      Bag.where Bool.(is_used && filter) (Bag.yield Bool.true')
    in
    Bag.where select (Bag.yield label)

  let copy_applications_stmt =
    (* FIXME rel *)
    let sql =
      Fmt.str
        "INSERT OR REPLACE INTO %s (entity, label)
         SELECT ?1, a.label
         FROM %s as a
         WHERE a.entity = ?2" (Table.name table) (Table.name table)
    in
    let stmt = Rel_sql.Stmt.(func sql @@ int @-> int @-> unit) in
    fun ~src ~dst -> stmt dst src

end

(* Queries *)

include Entity.Publicable_queries (Label)

(* Urls *)

module Url = struct
  open Result.Syntax

  let cancel = "cancel"
  let cancel_of_query query = Http.Query.find_first cancel query
  let cancel_to_query goto = match goto with
  | None -> None | Some goto ->
      Some (Http.Query.empty |> Http.Query.def cancel goto)

  type label = t
  type named_id = string option * id
  type t =
  | Create
  | Edit of id
  | Edit_new of { cancel : string option }
  | Index
  | Page of named_id
  | Update of id
  | View of id

  let id_meth u ms id =
    let* meth = Kurl.allow ms u in
    let* id = Res.Id.decode id in
    Ok (id, meth)

  let id_get req id = let* id, `GET = id_meth req Http.Method.[get] id in Ok id

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["api"; "ui"; "edit"; id] -> let* id = id_get u id in Kurl.ok (Edit id)
  | ["api"; "ui"; "view"; id] -> let* id = id_get u id in Kurl.ok (View id)
  | ["edit"; "new"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let cancel = cancel_of_query (Kurl.Bare.query u) in
      Kurl.ok (Edit_new { cancel })
  | [name; id] -> let* id = id_get u id in Kurl.ok (Page (Some name, id))
  | [id] ->
      let* id, meth = id_meth u Http.Method.[get;put] id in
      Kurl.ok @@ (match meth with `GET -> Page (None, id) | `PUT -> (Update id))
  | _ ->
      Kurl.no_match

  let html = ".html"
  let enc = function
  | Create -> Kurl.bare `POST [""]
  | Edit id -> Kurl.bare `GET ["api"; "ui"; "edit"; Res.Id.to_string id]
  | Edit_new { cancel } ->
      Kurl.bare `GET ["edit"; "new"] ?query:(cancel_to_query cancel)
  | Index -> Kurl.bare `GET [""] ~ext:html
  | Page (None, id) -> Kurl.bare `GET [Res.Id.to_string id] ~ext:html
  | Page (Some n, id) -> Kurl.bare `GET [n; Res.Id.to_string id] ~ext:html
  | Update id -> Kurl.bare `PUT [Res.Id.to_string id]
  | View id -> Kurl.bare `GET ["api"; "ui"; "view"; Res.Id.to_string id]

  let kind = Kurl.kind ~name:"label" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name l = Res.Named.name_of_string (name l)
  let page l = Kurl.v kind (Page (Some (res_name l), id l))
end
