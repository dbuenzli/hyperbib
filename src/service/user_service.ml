(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* It's a bit unclear whether we would not be better off disabling the
   whole service on Webapp.editable `No or `Unsafe. The nice thing
   with this is that if the mode changes and clients have previously
   served pages it all goes on smoothly for them. *)

let auth_disabled = "auth disabled"

let goto_or_service_path ~explain req ~goto =
  let goto = match goto with
  | None -> Http.Path.encode (Http.Req.service_path req)
  | Some goto -> goto
  in
  Http.Resp.redirect ~explain Http.found_302 goto

let authenticated ~explain app username ~goto = match goto with
| Some goto -> Http.Resp.redirect ~explain Http.found_302 goto
| None -> Page.resp (User_html.page (Webapp.page_gen app) ~username)

let authenticate app sess req ~goto = match Webapp.editable app with
| `No | `Unsafe ->
    Ok (sess, goto_or_service_path ~explain:auth_disabled req ~goto)
| `With_login ->
    Session.for_error None @@
    let* q = Http.Req.to_query req in
    let err e = Http.Resp.v ~explain:e Http.server_error_500 in
    let users_file = Hyperbib.Data_conf.users_file (Webapp.data_conf app) in
    let* users = Result.map_error err (User.load users_file) in
    let username = Http.Query.find User.Url.username_key q in
    let password = Http.Query.find User.Url.password_key q in
    let check = match username, password with
    | Some name, Some password -> User.check ~name ~password users
    | _ -> false
    in
    match check with
    | true ->
        let username = Option.get username and private_view = false in
        let sess = Webapp.Session.User { username; private_view } in
        let explain = "logged " ^ username in
        Ok (Some sess, authenticated ~explain app username ~goto)
    | false ->
        let log_user =
          Option.fold ~none:"" ~some:(Fmt.str " user %s") username
        in
        let explain = Fmt.str "bad credentials%s" log_user in
        let g = Webapp.page_gen app in
        let page = User_html.login g  ~msg:Uimsg.login_error ~goto in
        Ok (None, Page.resp ~explain ~status:Http.unauthorized_401 page)

let login app sess req ~goto = match Webapp.editable app with
| `No | `Unsafe ->
    Ok (sess, goto_or_service_path ~explain:auth_disabled req ~goto)
| `With_login ->
    match sess with
    | None | Some (Webapp.Session.Unsafe _) (* can't happen *) ->
        let g = Webapp.page_gen app in
        Ok (None, Page.resp (User_html.login g ~msg:Uimsg.login_descr ~goto))
    | Some (Webapp.Session.User { username; _ }) as s ->
        let explain = "already logged " ^ username in
        Ok (s, authenticated ~explain app username ~goto)

let logout app sess req ~goto = match Webapp.editable app with
| `No | `Unsafe ->
    Ok (None, goto_or_service_path ~explain:auth_disabled req ~goto)
| `With_login ->
    let explain = match sess with
    | None -> "already logged out"
    | Some (Webapp.Session.Unsafe _) (* can't happen *) -> auth_disabled
    | Some (Webapp.Session.User { username = u; _}) -> Fmt.str "%s logged out" u
    in
    Ok (None, goto_or_service_path ~explain req ~goto)

let view app sess req private' =
  (* Update session and ask for a page reload *)
  let headers = Http.Headers.(empty |> def (name "hc-reload") "true") in
  Result.ok @@
  match sess with
  | None ->
      None, Http.Resp.empty ~headers Http.unauthorized_401
  | Some (Webapp.Session.Unsafe _) ->
      let sess = Some (Webapp.Session.Unsafe { private_view = private' }) in
      sess, Http.Resp.empty ~headers Http.ok_200
  | Some (Webapp.Session.User s) ->
      let sess = Some (Webapp.Session.User { s with private_view = private'}) in
      sess, Http.Resp.empty ~headers Http.ok_200

let resp r app sess req = match (r : User.Url.t) with
| Login { goto } -> login app sess req ~goto
| Authenticate { goto } -> authenticate app sess req ~goto
| Logout { goto } -> logout app sess req ~goto
| View { private' } -> view app sess req private'

let v = Kurl.service User.Url.kind resp

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
