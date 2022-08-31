(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open Webs_kit

(* Sessions *)

module Session = struct
  type t =
  | Unsafe of { private_view : bool }
  | User of { username : string; private_view : bool }

  let private_view = function
  | Unsafe { private_view } -> private_view
  | User { private_view; _ } -> private_view

  let err s = Fmt.failwith "%a: invalid state" Fmt.(truncated ~max:4) s

  let encode_private_view private_view = if private_view then "t" else "f"
  let decode_private_view s = match s.[1] with
  | 'f' -> false | 't' -> true | _ -> err s

  let encode = function
  | Unsafe { private_view } -> "e" ^ encode_private_view private_view
  | User { username; private_view } ->
      "u" ^ encode_private_view private_view ^ username

  let decode s =
    try
      let len = String.length s in
      if len < 2 then err s else
      let private_view = decode_private_view s in
      begin match s.[0] with
      | 'e' -> if len <> 2 then err s else Ok (Unsafe { private_view })
      | 'u' ->
          let username = String.subrange ~first:2 s in
          Ok (User { username; private_view })
      | _ -> err s
      end
    with Failure e -> Error e

  let state = Session.State.v ~eq:( = ) ~encode ~decode ()

  type handler = (t, Session.client_stored_error) Session.handler
  let handler ~service_path ~private_key ~secure_cookie:secure =
    let name = "hyperbib" in
    let atts = Http.Cookie.atts ~secure ~path:service_path () in
    Session.client_stored ~atts ~private_key ~name ()
end

(* Service private key setup. This could be in a webs bazaar. *)

let setup_private_key ~file =
  let err_load_private_key file e =
    Fmt.str "@[<v>Service private key error: %s@,\
             To create a new one (logs out all users) delete file:@,%a@]"
      e Fmt.(code Fpath.pp_unquoted) file
  in
  let err_save_private_key file e =
    Fmt.str
      "[<v>Cannot save service private key:@,%a: %s@]" Fpath.pp_unquoted file e
  in
  let* exists = Os.File.exists file in
  match exists with
  | true ->
      Result.map_error (err_load_private_key file) @@
      let* key = Os.File.read file in
      Authenticatable.private_key_of_ascii_string key
  | false ->
      Result.map_error (err_save_private_key file) @@
      let key = Authenticatable.random_private_key_hs256 () in
      let save = Authenticatable.private_key_to_ascii_string key in
      let force = true and make_path = false in
      let* () = Os.File.write ~force ~make_path ~mode:0o600 file save in
      Ok key

(* Sub services *)

type sub =
  Service_env.t -> Session.t option -> Http.req ->
  (Session.t Webs_kit.Session.resp, Session.t Webs_kit.Session.resp) result

type sub_with_immutable_session =
  Service_env.t -> Session.t option -> Http.req -> (Http.resp, Http.resp) result

let sub_with_immutable_session service app sess req =
  Webs_kit.Session.for_result sess (service app sess req)

(* Service *)

type t = Service_env.t -> Http.req -> Http.resp

(* FIXME the following should still be further streamlined.
   Should we move what is in page_gen to service_env ? We need
   to make sure a Service_env can be created for static extraction. *)

let adjust_env_and_session env sess =
  (* This adjusts the session according to the webapp edition mode: we may
     still receive sessions from when the webapp was in a different mode.
     The page rendering parameters are also defined here. *)
  let env' caps ~auth_ui ~user_view ~private_data =
    let page_gen =
      let g = Service_env.page_gen env in
      let now = Ptime_clock.now () (* FIXME get last db update time. *) in
      let url_fmt = Page.Gen.url_fmt g in
      let b = Page.Gen.bibliography g in
      let testing = Page.Gen.testing g in
      Page.Gen.v ~now b url_fmt ~auth_ui ~user_view ~private_data ~testing
    in
    Service_env.adjust env caps page_gen
  in
  let user_view' ~private' = Some (if private' then `Private else `Public) in
  let private_data ~private' = private' in
  match Service_env.editable env with
  | `No ->
      let caps = User.Caps.none in
      let env = env' caps ~auth_ui:None ~user_view:None ~private_data:false in
      env, None (* Drop all sessions *)
  | `Unsafe ->
      let sess, private' = match sess with
      | None -> Some (Session.Unsafe { private_view = false }), false
      | Some (Session.Unsafe { private_view }) as s -> s, private_view
      | Some (Session.User { private_view; _ }) ->
          (* We are not showing a logout ui, so it seems better to turn
             that in an Unsafe_edit session, we retain the edit mode
             of the previously logged user. *)
          Some (Session.Unsafe { private_view }), private_view
      in
      let user_view = user_view' ~private' in
      let private_data = private_data ~private' in
      let caps = User.Caps.v ~edit:true in
      let env = env' caps ~auth_ui:None ~user_view ~private_data in
      env, sess
  | `With_login ->
      match sess with
      | None | Some (Session.Unsafe _) ->
          let caps = User.Caps.none in
          let auth_ui = Some `Login and user_view = None in
          let env = env' caps ~auth_ui ~user_view ~private_data:false in
          env, None
      | Some (Session.User { private_view; _ }) as sess ->
          let caps = User.Caps.v ~edit:true in
          let auth_ui = Some `Logout in
          let user_view = user_view' ~private':private_view in
          let private_data = private_data ~private':private_view in
          let env = env' caps ~auth_ui ~user_view ~private_data in
          env, sess

let v ~service_path ~private_key ~secure_cookie tree ~fallback env =
  let serve sess req =
    let sess = match sess with
    | Ok v -> v
    | Error e ->
        (* Simply drop the session for now *)
        let e = Webs_kit.Session.client_stored_error_message e in
        Log.err (fun m -> m "Session error: %s" e); None
    in
    let env, sess = adjust_env_and_session env sess in
    let sess, resp =
      Http.Resp.result @@
      let* req = Webs_kit.Session.for_error sess (Http.Req.clean_path req) in
      let sub = Kurl.find_service tree  (Kurl.Bare.of_req req) in
      let* sub = Webs_kit.Session.for_error sess sub in
      match sub with
      | Some sub -> sub env sess req
      | None -> (sub_with_immutable_session fallback) env sess req
    in
    let error = Page.error (Service_env.page_gen env) req in
    sess, Http.Resp.map_errors ~only_empty:true error resp
  in
  let handler = Session.handler ~service_path ~private_key ~secure_cookie in
  Webs_kit.Session.setup Session.state handler serve

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
