(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let setup_http_client () =
  (* We should eventually switch to libcurl *)
  let trace pid cmd =
    Log.debug (fun m -> m "%a" Webs_spawn_client.pp_trace (pid, cmd))
  in
  let c = Webs_spawn_client.make ~trace () in
  begin match c with
  | Ok _ -> () | Error e ->
      Log.warn @@ fun m ->
      m "@[<v>The app may not work properly, no HTTP client found:@,%s@]" e
  end;
  c


module Conf = struct
  type t =
    { log_level : Log.level;
      fmt_styler : Fmt.styler;
      http_client : (Http_client.t, string) result;
      app_dir : Fpath.t; }

  let make ~log_level ~fmt_styler ~app_dir ~http_client () =
    { log_level; fmt_styler; app_dir; http_client }

  let blobstore_path = Fpath.v "data/blobs"
  let db_path = Fpath.v "data/bib.sqlite3"
  let log_level c = c.log_level
  let fmt_styler c = c.fmt_styler
  let http_client c = c.http_client
  let app_dir c = c.app_dir
  let users_file c = Fpath.(c.app_dir / "users.json")
  let authentication_private_key c = Fpath.(c.app_dir / "auth.private")
  let static_dir c = Fpath.(c.app_dir / "static")
  let doi_cache_dir c = Fpath.(c.app_dir / "dois")
  let db_file c = Fpath.(c.app_dir // db_path)
  let db_backup_file c = Fpath.(db_file c + ".backup")
  let blobstore_dir c = Fpath.(c.app_dir // blobstore_path)
  let blobstore c = Blobstore.of_dir (blobstore_dir c)

  let find_app_dir = function
  | Some app_dir -> Ok app_dir
  | None ->
      let* dir = Os.Dir.cwd () in
      let* exists = Os.Path.exists Fpath.(dir // db_path) in
      if exists then Ok dir else
      Fmt.error
        "@[<v>Working directory is not an application directory.@,\
         @[%a: Use option %a to specify one or %a to use@ this@ directory@ \
         as@ an application directory.@]"
        Fmt.(st [`Fg `Yellow]) "Hint" Fmt.code "-a" Fmt.code "-a ."

  let with_cli ~log_level ~fmt_styler ~app_dir =
    Result.map_error (fun e -> `Msg e) @@
    (* I hate these three lines can't we do something better ? *)
    let log_level = B0_std_cli.get_log_level log_level in
    let fmt_styler = B0_std_cli.get_styler fmt_styler in
    B0_std_cli.setup fmt_styler log_level ~log_spawns:Log.Debug;
    let http_client = setup_http_client () in
    let* app_dir = find_app_dir app_dir in
    let* app_dir = Os.Path.realpath app_dir in
    Ok (make ~log_level ~fmt_styler ~app_dir ~http_client ())
end

let with_db conf f =
  let db_file = Conf.db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Db.with_open_schema Schema.v db_file f

let with_db_transaction conf kind f =
  let db_file = Conf.db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction kind db f

module Exit = struct
  open Cmdliner
  let ok = Os.Exit.code Cmd.Exit.ok
  let user_exists = Os.Exit.Code 1
  let conf_error = Os.Exit.Code 122
  let some_error = Os.Exit.Code Cmd.Exit.some_error
  module Info = struct
    let e c doc = Cmdliner.Cmd.Exit.info (Os.Exit.get_code c) ~doc
    let user_exists = e user_exists "on adding an existing user."
    let conf_error = e conf_error "on configuration error."
    let base_cmd = user_exists :: conf_error :: Cmd.Exit.defaults
  end
end

open Cmdliner
open Cmdliner.Term.Syntax

let fpath = Arg.conv' ~docv:"PATH" Fpath.(of_string, pp)
let common_man = []
let docs = Manpage.s_common_options

let conf =
  Term.term_result @@
  let+ log_level =
    let env = Cmd.Env.info "HYPERBIB_VERBOSITY" in
    B0_std_cli.log_level ~docs ~env ()
  and+ fmt_styler =
    let env = Cmd.Env.info "HYPERBIB_COLOR" in
    B0_std_cli.color ~docs ~env ()
  and+ app_dir =
    let doc = "Application directory." and docv = "APP_DIR" in
    let absent = "current working directory" in
    let env = Cmd.Env.info "HYPERBIB_APP_DIR" in
    Arg.(value & opt (some ~none:"." fpath) None &
         info ["a"; "app-dir"] ~doc ~docs ~docv ~env ~absent)
  in
  Conf.with_cli ~log_level ~fmt_styler ~app_dir

let cmd ?doc ?(man = []) ?(exits = []) name term =
  let man = [`Blocks man; `Blocks common_man] in
  let exits = List.append exits Exit.Info.base_cmd in
  Cmd.v (Cmd.info name ~exits ?doc ~man) term

let cmd_with_conf ?doc ?man ?exits name term =
  cmd ?doc ?man ?exits name Term.(term $ conf)

let cmd_group ?doc ?(man = []) ?(exits = []) ?default name cmds =
  let man = [`Blocks man; `Blocks common_man] in
  let exits = List.append exits Exit.Info.base_cmd in
  Cmd.group (Cmd.info name ~exits ?doc ~man) ?default cmds
