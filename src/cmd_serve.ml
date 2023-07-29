(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let log_startup c env =
  let app_dir = Hyperbib.Conf.app_dir (Service_env.conf env) in
  let l = Webs_http11_gateway.listener c in
  let service_path = Webs_http11_gateway.service_path c in
  Log.app (fun m ->
      m  "@[<v>Hyperbib %s database schema v%d@,\
               Application directory: %a@,\
               Listening on http://%a%a@]"
        Stamp.version
        Schema.version
        (Fmt.code Fpath.pp_unquoted) app_dir
        (Fmt.code Webs_listener.pp) l
        (Fmt.code Http.Path.pp) service_path);
  if (Service_env.editable env = `Unsafe) then
    Log.warn (fun m -> m "Anyone can edit the bibliography, no login required.")

let log_shutdown () =
  Log.app (fun m -> m "Everyone has been served! Goodbye.")

let setup_db ~read_only ~db_pool =
  Result.join @@ Db.string_error @@ Rel_pool.with' db_pool @@ fun db ->
  Db.ensure_schema ~read_only Schema.v db

let start_backup_thread ~conf ~db_pool ~backup_every_s =
  match backup_every_s with
  | None -> ()
  | Some every_s ->
      let backup = Hyperbib.Conf.db_backup_file conf in
      (* FIXME would be nice to stop that in finish *)
      ignore (Db.backup_thread db_pool ~every_s backup)

let finish ~db_pool =
  let errs es = String.concat "\n" (List.map Db.error_message es) in
  Result.map_error errs (Rel_pool.dispose db_pool)

let serve
    conf listener service_path max_connections backup_every_s editable
    insecure_cookie testing
  =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let* () = Hyperbib.Conf.ensure_data_dir conf in (* XXX *)
  let secure_cookie = not insecure_cookie in
  let read_only = editable = `No in
  let pool_size = max_connections + 1 (* backup thread *) in
  let db_file = Hyperbib.Conf.db_file conf in
  let db_pool = Db.pool ~read_only db_file ~size:pool_size in
  let* () = setup_db ~read_only ~db_pool in
  let service_path = Option.value ~default:[""] service_path in
  let* env =
    (* XXX still needs streamlining review when env get
       mutated. *)
    let* bib = Bibliography.get () in
    (* FIXME we need to get that data from somewhere. *)
    let scheme = "https" and authority = "philoclimate.ch" in
    let url_fmt = Kurl.Fmt.empty ~scheme ~authority ~root:service_path () in
    let url_fmt = Service_tree.url_fmt ~init:url_fmt in
    let page_gen =
      let auth_ui = None and user_view = None and private_data = false in
      let now = Ptime_clock.now () in
      Page.Gen.v ~now bib url_fmt ~auth_ui ~user_view ~private_data ~testing
    in
    Result.ok @@
    Service_env.v ~conf ~caps:User.Caps.none ~db_pool ~editable ~page_gen ()
  in
  let* service =
    let pk_file = Hyperbib.Conf.authentication_private_key conf in
    let* private_key = Service.setup_private_key ~file:pk_file in
    let tree = Service_tree.v and fallback = Static_file_service.v in
    Ok (Service.v ~service_path ~private_key ~secure_cookie tree ~fallback env)
  in
  let log = Http.Connector.Log.default ~trace:true () in
  let c =
    Webs_http11_gateway.make ~log ~listener ~service_path ~max_connections ()
  in
  log_startup c env;
  start_backup_thread ~conf ~db_pool ~backup_every_s;
  let* () = Webs_http11_gateway.serve c service in
  let* () = finish ~db_pool in
  log_shutdown ();
  Ok Hyperbib.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Serve the web application over HTTP/1.1"
let exits = Hyperbib.Exit.Info.base_cmd
let man = [
  `S Manpage.s_description;
  `P "The $(tname) serves the web application."; ]

let backup_every_s =
  let backup_every_s =
    let doc =
      "Make a stable file backup every $(docv) seconds (takes \
       twice the disk usage of the database and trice during backup)."
    in
    Arg.(value & opt int 3600 & info ["backup-every-s"] ~doc ~docv:"SECS")
  in
  let no_backup =
    let doc =
      "Do not periodically backup the database. \
       Takes over $(b,--backup-every-s)."
    in
    Arg.(value & flag & info ["no-backup"] ~doc)
  in
  let backup backup_every_s no_backup =
    if no_backup then None else (Some backup_every_s)
  in
  Term.(const backup $ backup_every_s $ no_backup)

let editable =
  let enum = ["no", `No; "with-login", `With_login; "unsafe", `Unsafe; ] in
  let mode = Arg.enum enum in
  let doc =
    Fmt.str "Make bibliography editable according to $(docv). \
             Must be one of %s. Respectively: no edits are allowed
             (the database is open in read only mode), a login is \
             required (see command $(b,add-user)), everyone can publicly \
             edit (not recommended for public facing services)."
      (Arg.doc_alts_enum enum)
  in
  Arg.(value & opt mode `With_login &
       info ["e"; "editable"] ~doc ~docv:"POLICY")

let testing =
  let doc =
    "Adds a top banner on the pages to indicate that this is a testing \
     install."
  in
  Arg.(value & flag & info ["testing"]  ~doc)

let insecure_cookie =
  let doc = "Do no use $(b,Secure) cookies for sessions. By default \
             $(b,Secure) cookie are used. Most browsers treat localhost \
             over HTTP as secure but others like Safari do not. Use \
             this option if you want to use the app with Safari via HTTP \
             on localhost."
  in
  Arg.(value & flag & info ["insecure-cookie"] ~doc)

let cmd =
  Cmd.v (Cmd.info "serve" ~doc ~exits ~man)
    Term.(const serve $ Hyperbib.Cli.conf $ Webs_cli.listener () $
          Webs_cli.service_path () $ Webs_cli.max_connections () $
          backup_every_s $ editable $ insecure_cookie $ testing)

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
