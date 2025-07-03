(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
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

type t =
  { http_client : (Http_client.t, string) result;
    app_dir : Fpath.t; }

let make ~app_dir ~http_client () =
  { app_dir; http_client }

let blobstore_path = Fpath.v "data/blobs"
let db_path = Fpath.v "data/bib.sqlite3"
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

let with_cli ~app_dir =
  (* I hate these three lines can't we do something better ? *)
  let http_client = setup_http_client () in
  let* app_dir = find_app_dir app_dir in
  let* app_dir = Os.Path.realpath app_dir in
  Ok (make ~app_dir ~http_client ())

let with_db conf f =
  let db_file = db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Db.with_open_schema Schema.v db_file f

let with_db_transaction conf kind f =
  let db_file = db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction kind db f
