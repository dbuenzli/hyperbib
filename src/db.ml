(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

(* Database *)

let dialect = Rel_sqlite3.dialect

type error = Rel_sqlite3.error
let error_message = Rel_sqlite3.Error.message
let error_rc_message e = Rel_sqlite3.Error.(code_to_string (code e))
let string_error = Rel_sqlite3.string_error

type t = Rel_sqlite3.t

let open' ?foreign_keys ?(read_only = false) file =
  let set_wal_mode db = Rel_sqlite3.exec_sql db "PRAGMA journal_mode=WAL;" in
  let mode = Rel_sqlite3.(if read_only then Read else Read_write_create) in
  let stmt_cache_size = 40 in
  let mutex = Rel_sqlite3.No and f = Fpath.to_string file in
  let* db = Rel_sqlite3.open' ?foreign_keys ~stmt_cache_size ~mutex ~mode f in
  let* () = Rel_sqlite3.busy_timeout_ms db 5000 in
  let* () = if read_only then Ok () else set_wal_mode db in
  Ok db

let close = Rel_sqlite3.close

let with_open ?foreign_keys ?read_only db_file f =
  let* db = open' ?foreign_keys ?read_only db_file in
  let finally () = Log.if_error ~use:() (close db |> string_error) in
  Fun.protect ~finally @@ fun () -> Ok (f db)

let with_open' ?foreign_keys ?read_only db_file f =
  Result.map_error (Fmt.str "%a: %s" Fpath.pp db_file) @@ string_error @@
  with_open ?foreign_keys ?read_only db_file f

let ensure_db_path db_path =
  Result.map ignore (Os.Dir.create ~make_path:true (Fpath.parent db_path))

(* Pool *)

type pool = (t, error) Rel_pool.t

let pool ?read_only file ~size =
  let create () = open' ?read_only file in
  let dispose = Rel_sqlite3.close in
  Rel_pool.create ~create ~dispose size

(* Backup *)

let stamped_backup_file file =
  let stamp =
    let (t : Unix.tm) = Unix.localtime (Unix.gettimeofday ()) in
    Fmt.str "-%04d-%02d-%02d-%02d%02d%02d"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  let p, ext = Fpath.cut_ext ~multi:true file in
  Fpath.add_ext (stamp ^ ext) p

let vaccum_into file db =
  let err e = Fmt.str "Vacuum into %a: %s" Fpath.pp_unquoted file e in
  Result.map_error err @@ Rel_sqlite3.string_error @@
  let vacuum = Rel_sql.Stmt.(func "VACUUM INTO ?1;" (text @-> unit)) in
  let vacuum = vacuum (Fpath.to_string file) in
  Rel_sqlite3.exec db vacuum

let backup file db =
  let* tmp = Os.Path.tmp ~dir:(Fpath.parent file) () in
  let* () = vaccum_into tmp db in
  Os.Path.rename ~force:true ~make_path:false tmp ~dst:file

let backup_thread pool ~every_s file =
  let handle_error v =
    let err e = Fmt.str "Backup thread: %s" e in
    Log.if_error ~use:() @@
    Result.map_error err @@ Result.join @@ Rel_sqlite3.string_error v
  in
  (* TODO we should do a clean termination. Do a control pipe
     and use select() *)
  let rec loop () =
    let backup file db =
      let* () = backup file db in
      Ok (Log.app (fun m -> m "Made stable database backup file."))
    in
    Rel_pool.with' pool (backup file) |> handle_error;
    Unix.sleep every_s;
    loop ()
  in
  Thread.create loop ()

let restore ~backup dst =
  let* () = Os.File.must_exist backup (* sqlite3 error message is subpar *) in
  Result.join @@ with_open' ~read_only:true backup @@ fun src ->
  string_error @@
  let* b = Rel_sqlite3.Backup.init ~dst ~src () in
  let* finished = Rel_sqlite3.Backup.step b () in
  assert (finished);
  Rel_sqlite3.Backup.finish b

(* Transactions *)

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]
let with_transaction k db f = Rel_sqlite3.with_transaction k db f

(* Schema handling *)

let setup db ~schema:s =
  Result.join @@ Rel_sqlite3.with_transaction `Immediate db @@ fun db ->
  let stmts = Rel_sql.create_schema dialect s in
  List.iter_stop_on_error (Rel_sqlite3.exec db) stmts

let clear db =
  Result.join @@ Rel_sqlite3.with_transaction `Immediate db @@ fun db ->
  let* (live, _) = Rel_sqlite3.schema_of_db db in
  let stmts = Rel_sql.drop_schema ~if_exists:() dialect live in
  List.iter_stop_on_error (Rel_sqlite3.exec db) stmts

let create_schema db s =
  Result.join @@ Rel_sqlite3.with_transaction `Immediate db @@ fun db ->
  let stmts = Rel_sql.create_schema dialect s in
  List.iter_stop_on_error (Rel_sqlite3.exec db) stmts

let ensure_schema ?(read_only = false) s db =
  let* live, _ = Rel_sqlite3.schema_of_db db |> string_error in
  let is_empty = Rel.Schema.tables live = [] in
  match is_empty with
  | true ->
      if not read_only then create_schema db s |> string_error else
      Fmt.error "Empty read-only database. Cannot setup schema."
  | false ->
      let* cs = Rel.Schema.changes ~src:live ~dst:s () in
      if cs = [] then Ok () else
      Fmt.error
        "@[<v>Live database and application schema do not match.@,\
         Use %a to inspect changes and upgrade@,the live schema.@]"
        Fmt.code "hyperbib db changes"

let schema = Rel_sqlite3.schema_of_db

let with_open_schema ?foreign_keys ?read_only schema db_file f =
  Result.join @@ string_error @@
  with_open ?foreign_keys ?read_only db_file @@ fun db ->
  let* () = ensure_schema schema db in
  Ok (f db)

(* Queries *)

let exec_sql db sql = Rel_sqlite3.exec_sql db sql
let exec db st = Rel_sqlite3.exec db st
let first = Rel_sqlite3.first
let fold = Rel_sqlite3.fold
let list db st = Rel_sqlite3.fold db st List.cons []

let insert db st =
  let* () = Rel_sqlite3.exec db st in
  (* FIXME might truncate, error *)
  Ok (Int64.to_int @@ Rel_sqlite3.last_insert_rowid db)

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Rel_sqlite3.fold db st add Id.Map.empty

let id_map_related_list ?order db rel_stmt ~id ~related ~related_by_id =
  let add r acc = match Id.Map.find_opt (related r) related_by_id with
  | None (* if the read is not in a transaction *) -> acc
  | Some p -> Id.Map.add_to_list (id r) p acc
  in
  let* m = Rel_sqlite3.fold db rel_stmt add Id.Map.empty in
  match order with
  | None -> Ok m
  | Some order -> Ok (Id.Map.map (List.sort order) m)

(* Statement debug *)

let show_sql ?(name = "") st =
  let name = if name = "" then "" else name ^ " " in
  Log.app (fun m -> m "@[<v>%sSQL:@,%a@]" name Rel_sql.Stmt.pp_src st);
  st

let show_plan ?(name = "") db st =
  match Rel_sqlite3.explain ~query_plan:true db st with
  | Error e ->
      let e = Rel_sqlite3.Error.message e in
      Log.err (fun m -> m "@[explain query plan: %s@]" e); st
  | Ok s ->
      let name = if name = "" then "" else name ^ " " in
      Log.app (fun m -> m "@[<v>%squery plan:@,%a@]" name Fmt.lines s); st

(* Webs convenience *)

let http_resp_error ?(retry_after_s = 2) e =
  let explain = Rel_sqlite3.Error.message e in
  match Rel_sqlite3.Error.code e with
  | e when e = Rel_sqlite3.Error.busy_timeout ->
      let dur = string_of_int retry_after_s in
      let headers = Http.Headers.empty |> Http.Headers.(def retry_after dur) in
      Http.Response.empty ~headers ~explain Http.Status.service_unavailable_503
  | _ ->
      Http.Response.empty ~explain Http.Status.server_error_500

let http_resp_error ?retry_after_s r =
  Result.map_error (http_resp_error ?retry_after_s) r

let first' db st = http_resp_error (first db st)
let list' db st = http_resp_error (list db st)
let exec' db st = http_resp_error (exec db st)
let insert' db st = http_resp_error (insert db st)
let with_transaction' k db f =
  http_resp_error (Rel_sqlite3.with_transaction k db f)
