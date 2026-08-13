(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

type t =
  { data_dir : Filepath.t;
    cache_dir : Filepath.t;
    http_client : (Http.Client.t, string) result; }

let make ~data_dir ~cache_dir ~http_client () =
  { data_dir; cache_dir; http_client }

let db_file_path = Filepath.v "bib/bib.sqlite3"
let blobstore_path = Filepath.v "bib/blobs"
let secrets_path = Filepath.v "secrets"

let data_dir c = c.data_dir
let authentication_secret_key_file c =
  Filepath.(c.data_dir // secrets_path /  "authentication.key")

let cache_dir c = c.cache_dir
let blobstore_dir c = Filepath.(c.data_dir // blobstore_path)
let db_file c = Filepath.(c.data_dir // db_file_path)
let db_backup_file c = Filepath.(db_file c + ".backup")
let doi_cache_dir c = Filepath.(c.cache_dir / "dois")
let http_client c = c.http_client
let static_dir c = Filepath.(c.data_dir / "static")
let users_file c = Filepath.(c.data_dir / "users.json")

let blobstore c = Blobstore.of_dir (blobstore_dir c)

let pp =
  let http_client c = Result.map Http.Client.id (http_client c) in
  Fmt.record
    [ Fmt.field "data-dir" data_dir Filepath.pp;
      Fmt.field "cache-dir" cache_dir Filepath.pp;
      Fmt.field "authentication-secret-key-file"
        authentication_secret_key_file Filepath.pp;
      Fmt.field "blobstore-dir" blobstore_dir Filepath.pp;
      Fmt.field "db-file" db_file Filepath.pp;
      Fmt.field "db-backup-file" db_backup_file Filepath.pp;
      Fmt.field "doi-cache-dir" doi_cache_dir Filepath.pp;
      Fmt.field "http-client" http_client Fmt.(result ~ok:string ~error:string);
      Fmt.field "static-dir" static_dir Filepath.pp;
      Fmt.field "users-file" users_file Filepath.pp ]

(* Discovery logic *)

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

let in_tooldir lookup_dir () =
  let* dir = lookup_dir () in
  Ok Filepath.(dir / tooldir)

let get_dir dir ~or_lookup:lookup_dir =
  let* dir = match dir with
  | Some dir -> Ok dir
  | None -> lookup_dir ()
  in
  let* exists = Os.Dir.exists dir in
  if not exists then Ok dir else Os.Path.realpath dir

let get_data_dir () =
  let* dir = Os.Dir.cwd () in
  let cwd_hyperbib = Filepath.(dir / tooldir) in
  let* exists = Os.Path.exists cwd_hyperbib in
  if exists then Ok cwd_hyperbib else
  let empty_is_none = true in
  let* dir = Os.Env.var' ~empty_is_none Filepath.of_string "HYPERBIB_DATA_DIR" in
  match dir with
  | Some dir -> Ok dir
  | None -> in_tooldir Os.Dir.data ()

let discover ~data_dir ~cache_dir =
  let http_client = setup_http_client () in
  let* data_dir = get_dir data_dir ~or_lookup:get_data_dir in
  let* cache_dir = get_dir cache_dir ~or_lookup:(in_tooldir Os.Dir.cache) in
  Ok (make ~data_dir ~cache_dir ~http_client ())

(* Using the database *)

let with_db config f =
  let db_file = db_file config in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Filepath.pp_unquoted db_file e) @@
  Db.with_open_schema Schema.v db_file f

let with_db_transaction config kind f =
  let db_file = db_file config in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Filepath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction kind db f
