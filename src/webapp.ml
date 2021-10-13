(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Application globals. *)

open Hyperbib.Std
open Result.Syntax
open Webs_kit

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

(* Application setup *)

type editable = [ `No | `With_login | `Unsafe ]
type t =
  { backup_every_s : int option;
    caps : User.Caps.t;
    conf : Hyperbib.Conf.t;
    data_conf : Hyperbib.Data_conf.t;
    db_pool : Db.pool;
    editable : editable;
    page_gen : Page.Gen.t;
    private_key : Authenticatable.private_key;
    secure_cookie : bool;
    service_path : Http.path;
    static_dir : Fpath.t; }

(* Properties *)

let backup_every_s a = a.backup_every_s
let caps a = a.caps
let conf a = a.conf
let data_conf a = a.data_conf
let editable a = a.editable
let page_gen a = a.page_gen
let private_key a = a.private_key
let secure_cookie a = a.secure_cookie
let service_path a = a.service_path
let static_dir a = a.static_dir
let url_fmt a = Page.Gen.url_fmt a.page_gen

let for_serve a caps ~auth_ui ~user_view ~private_data =
  let now = Ptime_clock.now () (* FIXME get last db update time. *) in
  let url_fmt = Page.Gen.url_fmt a.page_gen in
  let b = Page.Gen.bibliography a.page_gen in
  let testing = Page.Gen.testing a.page_gen in
  let page_gen =
    Page.Gen.v ~now b url_fmt ~auth_ui ~user_view ~private_data ~testing
  in
  { a with caps; page_gen }

(* Convenience db brackets *)

let with_db a f = Db.error_resp @@ Result.join @@ Ask_pool.with' a.db_pool f

let with_db' a f =
  Result.join @@ Db.error_resp @@ Ask_pool.with' a.db_pool f

let with_db_transaction k a f =
  Db.error_resp @@ Result.join @@ Result.join @@
  Ask_pool.with' a.db_pool (fun db -> Db.with_transaction k db f)

let with_db_transaction' k a f =
  Result.join @@ Db.error_resp @@ Result.join @@
  Ask_pool.with' a.db_pool (fun db -> Db.with_transaction k db f)

(* Serving *)

type service =
  t -> Session.t option -> Http.req -> Session.t Webs_kit.Session.result

type immutable_session_service =
  t -> Session.t option -> Http.req -> (Http.resp, Http.resp) result

let immutable_session_service service app sess req =
  Webs_kit.Session.for_result sess (service app sess req)

let setup_db a =
  Db.error_string @@
  if a.editable = `No then Ok () (* FIXME check schema version *) else
  let* () =
    Result.join @@ Ask_pool.with' a.db_pool @@ fun db ->
    let* () = Db.setup ~schema:Schema.tables ~drop_if_exists:false db in
    Ok ()
  in
  match a.backup_every_s with
  | None -> Ok ()
  | Some every_s ->
      let backup = Hyperbib.Data_conf.db_backup_file a.data_conf in
      (* FIXME would be nice to stop that in finish *)
      ignore (Db.backup_thread a.db_pool ~every_s backup);
      Ok ()

let setup
    ~backup_every_s ~conf ~data_conf ~editable ~max_connections
    ~secure_cookie ?service_path ~testing ()
  =
  let* () = Hyperbib.Data_conf.ensure_data_dir data_conf in
  let pk_file = Hyperbib.Data_conf.authentication_private_key data_conf in
  let* private_key = setup_private_key ~file:pk_file in
  let* bib = Bibliography.get () in
  let service_path = Option.value ~default:[""] service_path in
  let size = max_connections in
  let db_file = Hyperbib.Data_conf.db_file data_conf in
  let db_pool = Db.pool ~read_only:(editable = `No) db_file ~size in
  let static_dir = Hyperbib.Data_conf.static_dir data_conf in
  let page_gen = (* FIXME maybe we shouldn't have that here. *)
    let url_fmt = Kurl.Fmt.empty ~root:service_path () in
    let auth_ui = None and user_view = None and private_data = false in
    let now = Ptime_clock.now () in
    Page.Gen.v ~now bib url_fmt ~auth_ui ~user_view ~private_data ~testing
  in
  let caps = User.Caps.none in
  let a =
    { backup_every_s; caps; conf; data_conf; db_pool; editable;
      page_gen; private_key; secure_cookie; service_path; static_dir; }
  in
  let* () = setup_db a in Ok a

let serve a ~url_fmt service =
  let handle_session_error = function
  | Ok v -> v
  | Error e ->
      (* Simply drop the session for now *)
      let e = Webs_kit.Session.client_stored_error_message e in
      Log.err (fun m -> m "Session error: %s" e); None
  in
  let serve a sess req =
    let sess = handle_session_error sess in
    service a sess req
  in
  let page_gen =
    (* FIXME *)
    let url_fmt = url_fmt ~init:(Page.Gen.url_fmt a.page_gen) in
    let now = Page.Gen.now a.page_gen in
    let bib_conf = Page.Gen.bibliography a.page_gen in
    let auth_ui = Page.Gen.auth_ui a.page_gen in
    let user_view = Page.Gen.user_view a.page_gen in
    let private_data = Page.Gen.private_data a.page_gen in
    let testing = Page.Gen.testing a.page_gen in
    Page.Gen.v
      ~now bib_conf url_fmt ~auth_ui ~user_view ~private_data ~testing
  in
  let a = { a with page_gen } in
  let service_path = a.service_path and private_key = a.private_key in
  let secure_cookie = a.secure_cookie in
  let handler = Session.handler ~service_path ~private_key ~secure_cookie in
  Webs_kit.Session.setup Session.state handler (serve a)

let finish a =
  let errs es = String.concat "\n" (List.map Db.error_message es) in
  Result.map_error errs (Ask_pool.dispose a.db_pool)

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
