(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

type t =
  { app_dir : Fpath.t;
    cache_dir : Fpath.t;
    http_client : (Http_client.t, string) result; }

let make ~app_dir ~cache_dir ~http_client () =
  { app_dir; cache_dir; http_client }

let db_path = Fpath.v "data/bib.sqlite3"
let blobstore_path = Fpath.v "data/blobs"

let app_dir c = c.app_dir
let authentication_private_key c = Fpath.(c.app_dir / "auth.private")
let cache_dir c = c.cache_dir
let blobstore_dir c = Fpath.(c.app_dir // blobstore_path)
let db_file c = Fpath.(c.app_dir // db_path)
let db_backup_file c = Fpath.(db_file c + ".backup")
let doi_cache_dir c = Fpath.(c.cache_dir / "dois")
let http_client c = c.http_client
let static_dir c = Fpath.(c.app_dir / "static")
let users_file c = Fpath.(c.app_dir / "users.json")

let blobstore c = Blobstore.of_dir (blobstore_dir c)

let pp =
  let http_client c = Result.map Http_client.id (http_client c) in
  Fmt.record
    [ Fmt.field "app-dir" app_dir Fpath.pp;
      Fmt.field "cache-dir" cache_dir Fpath.pp;
      Fmt.field "authentication-private-key"
        authentication_private_key Fpath.pp;
      Fmt.field "blobstore-dir" blobstore_dir Fpath.pp;
      Fmt.field "db-file" db_file Fpath.pp;
      Fmt.field "db-backup-file" db_backup_file Fpath.pp;
      Fmt.field "doi-cache-dir" doi_cache_dir Fpath.pp;
      Fmt.field "http-client" http_client Fmt.(result ~ok:string ~error:string);
      Fmt.field "static-dir" static_dir Fpath.pp;
      Fmt.field "users-file" users_file Fpath.pp ]

(* Discovery logic *)

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

let tooldir = "hyperbib"

let get_dir ~dir lookup_dir =
  let* dir = match dir with
  | Some dir -> Ok dir
  | None ->
      let* dir = lookup_dir () in
      Ok Fpath.(dir / tooldir)
  in
  let* exists = Os.Dir.exists dir in
  if not exists then Ok dir else Os.Path.realpath dir

let discover ~app_dir ~cache_dir =
  let http_client = setup_http_client () in
  let* app_dir = find_app_dir app_dir in
  let* app_dir = Os.Path.realpath app_dir in
  let* cache_dir = get_dir ~dir:cache_dir Os.Dir.cache in
  Ok (make ~app_dir ~cache_dir ~http_client ())

(* Using the database *)

let with_db config f =
  let db_file = db_file config in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Db.with_open_schema Schema.v db_file f

let with_db_transaction config kind f =
  let db_file = db_file config in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction kind db f
