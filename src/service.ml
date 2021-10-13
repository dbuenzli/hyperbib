(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let immutable_session s = Kurl.map_service Webapp.immutable_session_service s

let tree =
  Kurl.empty ()
  |> Kurl.bind [""] (immutable_session Bibliography_service.v)
  |> Kurl.bind ["labels"] (immutable_session Label_service.v)
  |> Kurl.bind ["subjects"] (immutable_session Subject_service.v)
  |> Kurl.bind ["containers"] (immutable_session Container_service.v)
  |> Kurl.bind ["persons"] (immutable_session Person_service.v)
  |> Kurl.bind ["references"] (immutable_session Reference_service.v)
  |> Kurl.bind ["years"] (immutable_session Year_service.v)
  |> Kurl.bind ["users"] User_service.v

let url_fmt ~init:uf =
  let uf = Kurl.Fmt.bind_tree tree uf in
  let uf = Kurl.Fmt.bind [""] Static_file.Url.kind uf in
  (* FIXME add to Fmt a bare formatter at the root by default.
     This allows for untyped formatting which we use in services
     to derive a self from referer rather than assume like is now in many
     cases for fragments that the request comes from a specific page. *)
  let uf = Kurl.Fmt.bind [""] Page.error_url_kind uf in
  uf

(* FIXME still quite unhappy what goes where betwen
   Webapp/Webapp.Session/Page.Gen. The following is quite ugly
   streamline. *)

let adjust_app_and_session app sess =
  (* This adjusts the session according to the webapp edition mode: we may
     still receive sessions from when the webapp was in a different mode.
     The page rendering paramateres are also defined here. *)
  let app' = Webapp.for_serve app in
  let user_view' ~private' = Some (if private' then `Private else `Public) in
  let private_data ~private' = private' in
  match Webapp.editable app with
  | `No ->
      let caps = User.Caps.none in
      let app = app' caps ~auth_ui:None ~user_view:None ~private_data:false in
      app, None (* Drop all sessions *)
  | `Unsafe ->
      let sess, private' = match sess with
      | None -> Some (Webapp.Session.Unsafe { private_view = false }), false
      | Some (Webapp.Session.Unsafe { private_view }) as s -> s, private_view
      | Some (Webapp.Session.User { private_view; _ }) ->
          (* We are not showing a logout ui, so it seems better to turn
             that in an Unsafe_edit session, we retain the edit mode
             of the previously logged user. *)
          Some (Webapp.Session.Unsafe { private_view }), private_view
      in
      let user_view = user_view' ~private' in
      let private_data = private_data ~private' in
      let caps = User.Caps.v ~edit:true in
      let app = app' caps ~auth_ui:None ~user_view ~private_data in
      app, sess
  | `With_login ->
      match sess with
      | None | Some (Webapp.Session.Unsafe _) ->
          let caps = User.Caps.none in
          let auth_ui = Some `Login and user_view = None in
          let app = app' caps ~auth_ui ~user_view ~private_data:false in
          app, None
      | Some (Webapp.Session.User { private_view; _ }) as sess ->
          let caps = User.Caps.v ~edit:true in
          let auth_ui = Some `Logout in
          let user_view = user_view' ~private':private_view in
          let private_data = private_data ~private':private_view in
          let app = app' caps ~auth_ui ~user_view ~private_data in
          app, sess

let error_pages app req resp =
  (* FIXME webs body and content length business this shows we do it wrong. *)
  let t = Page.error (Webapp.page_gen app) req resp in
  let explain = Resp.explain resp in
  let headers = Http.Headers.undef Http.content_length (Resp.headers resp) in
  let status = Resp.status resp in
  Page.resp ~explain ~headers ~status t

let v app sess req =
  let app, sess = adjust_app_and_session app sess in
  let sess, resp =
    Resp.result @@
    let* req = Session.for_error sess (Req.clean_path req) in
    let service = Kurl.find_service tree (Kurl.Bare.of_req req) in
    let* service = Session.for_error sess service in
    match service with
    | Some service -> service app sess req
    | None -> Static_file_service.v app sess req
  in
  sess, Resp.map_errors ~only_empty:true (error_pages app req) resp

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

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
