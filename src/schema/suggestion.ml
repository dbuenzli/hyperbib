(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Rel

module Suggestion = struct
  type id = Id.t
  type t =
    { id : id;
      timestamp : int;
      doi : Doi.t;
      suggestion : string;
      comment : string;
      email : string }

  let v ~id ~timestamp ~doi ~suggestion ~comment ~email () =
    { id; timestamp; doi; suggestion; comment; email }

  let row id timestamp doi suggestion comment email =
    { id; timestamp; doi; suggestion; comment; email }

  let new' =
    { id = 0; timestamp = 0; doi = ""; suggestion = "";  comment = "";
      email = ""; }

  let id s = s.id
  let timestamp s = s.timestamp
  let doi s = s.doi
  let suggestion s = s.suggestion
  let comment s = s.comment
  let email s = s.email

  (* Table *)

  let id' = Col.v "id" Type.Int id
  let timestamp' = Col.v "timestamp" Type.Int timestamp
  let doi' = Col.v "doi" Type.Text doi
  let suggestion' = Col.v "suggestion" Type.Text suggestion
  let comment' = Col.v "comment" Type.Text comment
  let email' = Col.v "email" Type.Text email
  let table =
    let primary_key = [Col.V id'] in
    let indices = [ Table.index [Col.V doi']] in
    let row =
      Row.(unit row * id' * timestamp' * doi' * suggestion' * comment' *
           email')
    in
    Table.v "suggestion" row ~primary_key ~indices
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
  let is_doi = Text.(not (doi = empty) && doi = r #. doi') in
  Bag.where is_doi (Bag.yield r)

module Url = struct
  open Result.Syntax

  type t =
  | Create
  | Confirm_delete of id
  | Delete of id
  | Fill_in
  | Index
  | Page of { id : id; created : bool }
  | View_fields of id

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Meth.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["part"; "confirm-delete"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Confirm_delete id)
  | ["part"; "fill-in"] ->
      let* `POST = Kurl.allow Http.Meth.[post] u in
      Kurl.ok Fill_in
  | ["part"; "view-fields"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (View_fields id)
  | [id] ->
      let* meth, id = Entity.Url.meth_id u Http.Meth.[get; delete] id in
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
      Kurl.bare `GET ["part"; "confirm-delete"; Res.Id.to_string id]
  | Delete id -> Kurl.bare `DELETE [Res.Id.to_string id]
  | Fill_in -> Kurl.bare `POST ["part"; "fill-in"]
  | Index -> Kurl.Bare.v `GET [""] ~ext:html
  | Page {id; created} ->
      let query = match created with
      | true -> Http.Query.add "created" "" Http.Query.empty
      | false -> Http.Query.empty
      in
      Kurl.bare `GET [Res.Id.to_string id] ~query ~ext:html
  | View_fields id ->
      Kurl.bare `GET ["part"; "view-fields"; Res.Id.to_string id]

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers

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
