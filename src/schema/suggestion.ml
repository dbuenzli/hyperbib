(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

module Suggestion = struct
  module Id = Rel_kit.Id.MakeInt ()
  type t =
    { id : Id.t;
      timestamp : int;
      doi : Doi.t option;
      suggestion : string;
      comment : string;
      email : string }

  let make ~id ~timestamp ~doi ~suggestion ~comment ~email () =
    { id; timestamp; doi; suggestion; comment; email }

  let row id timestamp doi suggestion comment email =
    { id; timestamp; doi; suggestion; comment; email }

  let new' =
    { id = Id.zero; timestamp = 0; doi = None; suggestion = "";  comment = "";
      email = ""; }

  let id s = s.id
  let timestamp s = s.timestamp
  let doi s = s.doi
  let suggestion s = s.suggestion
  let comment s = s.comment
  let email s = s.email

  (* Table *)

  let id' = Col.make "id" Id.type' id
  let timestamp' = Col.make "timestamp" Type.int timestamp
  let doi' = Col.make "doi" Type.(option Schema_kit.Doi_rel.t) doi
  let suggestion' = Col.make "suggestion" Type.text suggestion
  let comment' = Col.make "comment" Type.text comment
  let email' = Col.make "email" Type.text email
  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    let indices = [ Table.Index.make [Col.Def doi']] in
    Table.make "suggestion" ~primary_key ~indices @@
    Row.(unit row * id' * timestamp' * doi' * suggestion' * comment' * email')
end

include Suggestion
include Entity.Identifiable_queries (Suggestion)

open Rel_query.Syntax

let list =
  let* r = Bag.table table in
  Bag.yield r

let list_stmt = Rel_query.Sql.(func @@ ret (Table.row table) list)

let find_doi doi =
  let* r = Bag.table table in
  let is_doi = Option.(equal (some Schema_kit.Doi_rel.t doi) (r #. doi')
                         ~eq:Schema_kit.Doi_rel.equal)
  in
  Bag.where is_doi (Bag.yield r)

module Url = struct
  open Result.Syntax

  type t =
  | Create
  | Confirm_delete of Id.t
  | Delete of Id.t
  | Fill_in
  | Index
  | Page of { id : Id.t; created : bool }
  | View_fields of Id.t

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["part"; "confirm-delete"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Confirm_delete id)
  | ["part"; "fill-in"] ->
      let* `POST = Kurl.allow Http.Method.[post] u in
      Kurl.ok Fill_in
  | ["part"; "view-fields"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (View_fields id)
  | [id] ->
      let* meth, id =
        Entity.Url.meth_id (module Id) u Http.Method.[get; delete] id
      in
      let url = match meth with
      | `DELETE -> Delete id
      | `GET ->
          let created = Http.Query.mem "created" (Kurl.Bare.query u) in
          Page { id; created }
      in
      Kurl.ok url
  | _ -> Kurl.no_match

  let html = ".html"
  let enc = function
  | Create -> Kurl.bare `POST [""]
  | Confirm_delete id ->
      Kurl.bare `GET ["part"; "confirm-delete"; Id.to_string id]
  | Delete id -> Kurl.bare `DELETE [Id.to_string id]
  | Fill_in -> Kurl.bare `POST ["part"; "fill-in"]
  | Index -> Kurl.Bare.v `GET [""] ~ext:html
  | Page {id; created} ->
      let query = match created with
      | true -> Http.Query.empty |> Http.Query.def "created" ""
      | false -> Http.Query.empty
      in
      Kurl.bare `GET [Id.to_string id] ~query ~ext:html
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Id.to_string id]

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
end
