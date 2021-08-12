(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

type t = Ask_sqlite3.t
type error = Ask_sqlite3.error
type pool = (t, error) Ask_pool.t

let error_message = Ask_sqlite3.Error.message
let error_string = Ask_sqlite3.error_string

let open' ?foreign_keys ?(read_only = false) file =
  let mode = Ask_sqlite3.(if read_only then Read else Read_write_create) in
  let stmt_cache_size = 40 in
  let mutex = Ask_sqlite3.No and f = Fpath.to_string file in
  let* db = Ask_sqlite3.open' ?foreign_keys ~stmt_cache_size ~mutex ~mode f in
  let* () = Ask_sqlite3.busy_timeout_ms db 5000 in
  Ok db

let setup db ~schema:s ~drop_if_exists =
  let* () = Ask_sqlite3.exec_sql db "PRAGMA journal_mode=WAL" in
  Result.join @@ Ask_sqlite3.with_transaction `Immediate db @@ fun db ->
  let* () = Ask_sqlite3.exec_once db (Sql.create_schema ~drop_if_exists s) in
  Ok ()

let pool ?read_only file ~size =
  let create () = open' ?read_only file in
  let dispose = Ask_sqlite3.close in
  Ask_pool.create ~create ~dispose size

let vaccum_into file db =
  let err e = Fmt.str "Vacuum into %a: %s" Fpath.pp_unquoted file e in
  Result.map_error err @@ Ask_sqlite3.error_string @@
  let vacuum = Sql.Stmt.(func "VACUUM INTO ?1" (text @-> unit)) in
  let vacuum = vacuum (Fpath.to_string file) in
  Ask_sqlite3.exec db vacuum

let backup file db =
  let tmp = Fpath.(file + ".tmp") in
  let* _exists = Os.File.delete tmp in
  let* () = vaccum_into tmp db in
  let* () = Os.Path.rename ~force:true ~make_path:false ~src:tmp file in
  Ok (Log.app (fun m -> m "Made stable database backup file."))

let backup_thread pool ~every_s file =
  let handle_error v =
    let err e = Fmt.str "Backup thread: %s" e in
    Log.if_error ~use:() @@
    Result.map_error err @@ Result.join @@ Ask_sqlite3.error_string v
  in
  (* TODO we should do a clean termination. Do a control pipe
     and use select() *)
  let rec loop () =
    Ask_pool.with' pool (backup file) |> handle_error;
    Unix.sleep every_s;
    loop ()
  in
  Thread.create loop ()

(* Debug convience *)

let show_sql ?(name = "") st =
  let name = if name = "" then "" else name ^ " " in
  Log.app (fun m -> m "@[<v>%sSQL:@,%a@]" name Sql.Stmt.pp_src st);
  st

let show_plan ?(name = "") db st =
  match Ask_sqlite3.explain ~query_plan:true db st with
  | Error e ->
      let e = Ask_sqlite3.Error.message e in
      Log.err (fun m -> m "@[explain query plan: %s@]" e); st
  | Ok s ->
      let name = if name = "" then "" else name ^ " " in
      Log.app (fun m -> m "@[<v>%squery plan:@,%a@]" name Fmt.lines s); st

(* Queries and convenience *)

let exec db st = Ask_sqlite3.exec db st
let first = Ask_sqlite3.first
let fold = Ask_sqlite3.fold
let list db st = Ask_sqlite3.fold db st List.cons []

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]
let with_transaction k db f = Ask_sqlite3.with_transaction k db f

let insert db st =
  let* () = Ask_sqlite3.exec db st in
  (* FIXME might truncate, error *)
  Ok (Int64.to_int @@ Ask_sqlite3.last_insert_rowid db)

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Ask_sqlite3.fold db st add Id.Map.empty

let id_map_related_list ?order db rel_stmt ~id ~related ~related_by_id =
  let add r acc = match Id.Map.find_opt (related r) related_by_id with
  | None (* if the read is not in a transaction *) -> acc
  | Some p -> Id.Map.add_to_list (id r) p acc
  in
  let* m = Ask_sqlite3.fold db rel_stmt add Id.Map.empty in
  match order with
  | None -> Ok m
  | Some order -> Ok (Id.Map.map (List.sort order) m)

(* Webs convenience *)

let error_to_resp ?(retry_after_s = 2) e =
  let explain = Ask_sqlite3.Error.message e in
  match Ask_sqlite3.Error.code e with
  | e when e = Ask_sqlite3.Error.busy_timeout ->
      let dur = string_of_int retry_after_s in
      let headers = Http.Headers.(empty |> add Http.retry_after dur) in
      Resp.v ~headers ~explain Http.service_unavailable_503
  | _ ->
      Resp.v ~explain Http.server_error_500

let error_resp ?retry_after_s r =
  Result.map_error (error_to_resp ?retry_after_s) r

let first' db st = error_resp (first db st)
let list' db st = error_resp (list db st)
let exec' db st = error_resp (exec db st)
let insert' db st = error_resp (insert db st)
let with_transaction' k db f = error_resp (Ask_sqlite3.with_transaction k db f)

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
