(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let log_making_backup file =
  Log.stdout (fun m -> m "Making backup to %a" (Fmt.code' Fpath.pp) file)

let log_restore_backup backup db =
  Log.stdout (fun m -> m "@[<v>Restoring backup %a@,into %a@]"
              (Fmt.code' Fpath.pp) backup (Fmt.code' Fpath.pp) db)

let make_backup db_file db =
  let backup = Db.stamped_backup_file db_file in
  log_making_backup backup; Db.backup backup db

(* Backup *)

let backup ~config ~file =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let db_file = Hyperbib_config.db_file config in
  let file = match file with
  | None -> Db.stamped_backup_file db_file | Some file -> file
  in
  Result.join @@ Db.string_error @@ Db.with_open db_file @@ fun db ->
  let* () = log_making_backup file; Db.backup file db in
  Ok Hyperbib_cli.Exit.ok

(* Changes *)

let do_changes (col_renames, table_renames) db =
  Log.stdout (fun m -> m "Changing live database schema…");
  let* (live, issues) = Db.schema db |> Db.string_error in
  let src = live and dst = Schema.v in
  let* cs = Rel.Schema.changes ~col_renames ~table_renames ~src ~dst () in
  let trans, stmts = Rel_sql.schema_changes Rel_sqlite3.dialect cs in
  if trans then
    Result.join @@ Db.string_error @@
    Db.with_transaction `Immediate db @@ fun db ->
    List.iter_stop_on_error (Db.exec db) stmts |> Db.string_error
  else
  List.iter_stop_on_error (Db.exec db) stmts |> Db.string_error

let changes
    ~config ~renames:(col_renames, table_renames as r) ~format ~exec
    ~no_backup
  =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let db_file = Hyperbib_config.db_file config in
  Result.join @@ Db.string_error @@ Db.with_open db_file @@ fun db ->
  let* (live, issues) = Db.schema db |> Db.string_error in
  List.iter (fun i -> Log.warn (fun m -> m "%s" i)) issues;
  let src = live and dst = Schema.v in
  let* cs = Rel.Schema.changes ~col_renames ~table_renames ~src ~dst () in
  let* () = match exec with
  | true when cs = [] -> Log.stdout (fun m -> m "Nothing to execute."); Ok ()
  | false when cs = [] -> Ok ()
  | true ->
      (* We cannot be a transaction to do the backup, so we do it
         here. We then recompute the changes. *)
      let* () = if no_backup then Ok () else make_backup db_file db in
      do_changes r db
  | false ->
      match format with
      | None | Some `Sqlite3 ->
          let _, stmts = Rel_sql.schema_changes Rel_sqlite3.dialect cs in
          Log.stdout (fun m -> m "@[<v>%a@]"
                         (Fmt.list Rel_sql.Stmt.pp_src) stmts);
          Ok ()
      | Some `Pseudo_sql ->
          let pp_changes = Fmt.list Rel.Schema.pp_change in
          Log.stdout (fun m -> m "@[<v>%a@]" pp_changes cs); Ok ()
  in
  Ok Hyperbib_cli.Exit.ok

let restore ~config ~backup ~last =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let db_file = Hyperbib_config.db_file config in
  let* backup = match backup with
  | None when last -> Ok (Hyperbib_config.db_backup_file config)
  | Some backup -> Ok backup
  | None ->
      Fmt.error
        "@[<v>No backup specified.@,Specify an %a file on the \
         command line or@,use option %a to use the latest automated backup.@]"
        Fmt.code ".sqlite3" Fmt.code "-l"
  in
  Result.join @@ Db.with_open' db_file @@ fun dst ->
  let () = log_restore_backup backup db_file in
  let* () = Db.restore ~backup dst in
  Ok Hyperbib_cli.Exit.ok

(* Reset *)

let reset ~config ~no_backup (* populate *) =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@ Result.join @@
  let db_file = Hyperbib_config.db_file config in
  let* exists = Os.File.exists db_file in
  Db.string_error @@ Db.with_open db_file @@ fun db ->
  let* () = if no_backup || not exists then Ok () else make_backup db_file db in
  let* () = Db.clear db |> Db.string_error in
  let* () = Db.ensure_schema Schema.v db in
(*  let* () = if populate then do_populate db else Ok () in *)
  Ok Hyperbib_cli.Exit.ok

(* Schema *)

let output_schema ~format s = match format with
| `Dot rankdir ->
    Log.stdout (fun m -> m "@[%a@]" (Rel.Schema.pp_dot ~rankdir) s);
| `Sqlite3 ->
    let stmts = Rel_sql.create_schema Db.dialect s in
    Log.stdout (fun m -> m "@[<v>%a@]" (Fmt.list Rel_sql.Stmt.pp_src) stmts);
| `Ocaml kind ->
    Log.stdout (fun m -> m "@[%a@]" (Rel.Schema.pp_ocaml kind) s)

let schema ~config ~which ~format =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let* () = match which with
  | `App -> output_schema ~format Schema.v; Ok ()
  | `Live ->
      Db.string_error @@ Result.join @@
      Db.with_open (Hyperbib_config.db_file config) @@ fun db ->
      let* live, issues = Db.schema db in
      output_schema ~format live;
      List.iter (fun i -> Log.warn (fun m -> m "%a" Fmt.lines i)) issues;
      Ok ()
  in
  Ok Hyperbib_cli.Exit.ok

(* SQL prompt *)

let sql ~config ~args =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let db_file = Hyperbib_config.db_file config in
  let args = match List.rev args with
  | [] -> Cmd.path db_file
  | a :: _ when String.length a > 1 && a.[0] = '-' (* is an option *) ->
      Cmd.(list args %% path db_file)
  | sql :: ropts -> Cmd.(list (List.rev ropts) %% path db_file % sql)
  in
  let* sqlite3 = Os.Cmd.get (Cmd.tool "sqlite3") in
  Os.Exit.exit @@ Os.Exit.execv Cmd.(sqlite3 % "-header" %% args)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let backup_cmd =
  let doc = "Make a backup of the database" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) makes a backup of the live database."; ]
  in
  Cmd.make (Cmd.info "backup" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ file =
    let doc =
      "The backup file. If unspecified a new timestamped file is written \
       in data directory of the application directory."
    in
    Arg.(value & pos 0 (some More_cli.filepath) None & info [] ~doc)
  in
  backup ~config ~file

let changes_cmd =
  let doc = "Compare live database and application schema" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the steps needed to bring the live database \
        schema to the one expected by the application.";
    `P "Table and column renames are not detected automatically and need \
        to be specified via the $(b,--rename) option.";
    `P "$(b,WARNING) always make a database backup before executing \
        these steps, $(b,--exec) does that by default."; ]
  in
  Cmd.make (Cmd.info "changes" ~doc ~man) @@
  let+ config = Hyperbib_cli.config and+ renames = Rel_cli.renames ()
  and+ format =
    let formats = [ "pseudo-sql", `Pseudo_sql; "sqlite3", `Sqlite3; ] in
    let doc = Printf.sprintf
        "Changes output format, by default outputs SQL data definitions for \
         SQLite. $(docv) must be %s. $(b,pseudo-sql) is an ad-hoc SQL format \
         used for understanding."
        (Arg.doc_alts_enum formats)
    in
    let docv = "FMT" in
    Arg.(value & opt (some (enum formats)) None & info ["format"] ~doc ~docv)
  and+ exec =
    let doc = "Execute the steps." in
    Arg.(value & flag & info ["exec"] ~doc)
  and+ no_backup =
    let doc =
      "Do not perform a database backup before executing. $(b,WARNING) \
       this may be dangerous for your data."
    in
    Arg.(value & flag & info ["no-backup"] ~doc)
  in
  changes ~config ~renames ~format ~exec ~no_backup

let restore_cmd =
  let doc = "Restore a database backup" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) restores a backup of the database."; ]
  in
  Cmd.make (Cmd.info "restore" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ backup =
    let doc = "$(docv) is the backup file to restore." in
    let docv = "BACKUP.sqlite3" in
    Arg.(value & pos 0 (some More_cli.filepath) None & info [] ~doc ~docv)
  and+ last =
    let doc = "Use the last automated backup." in
    Arg.(value & flag & info ["l"; "last"] ~doc)
  in
  restore ~config ~backup ~last

let reset_cmd =
  let doc = "Reset the database" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) clears the database and creates the app schema. A backup is \
        made if the database exists, use $(b,--no-backup) to prevent that."; ]
  in
  Cmd.make (Cmd.info "reset" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ no_backup =
    let doc =
      "Do not perform a database backup before reseting. $(b,WARNING) this \
       destroys all data."
    in
    Arg.(value & flag & info ["no-backup"] ~doc)
  in
(*  let populate =
    let doc = "Populate the tables with basic app data." in
    Arg.(value & flag & info ["p"; "populate"] ~doc)
  in *)
  reset ~config ~no_backup

let schema_cmd =
  let doc = "Output the app or live database schema" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the app or live database schema in various formats."]
  in
  Cmd.make (Cmd.info "schema" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ format = Rel_cli.schema_format ~default:`Sqlite3 ()
  and+ which =
    let e = ["app", Some `App; "live", Some `Live] in
    let doc = Fmt.str
        "Schema to output. Must be %s. $(b,app) is the schema assumed by \
         the software. $(b,live) is the schema of the database."
        (Arg.doc_alts_enum e)
    in
    let docv = "WHICH" in
    Arg.(required & pos 0 (Arg.enum e) None & info [] ~doc ~docv)
  in
  schema ~config ~which ~format

let sql_cmd =
  let doc = "Get an SQL prompt on the database" in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(cmd) [$(i,OPTION)]… $(b,--) $(i,OPTION)… [$(i,SQL)]";
    `S Manpage.s_description;
    `P "$(cmd) gets you an interactive SQL prompt to interact with \
        the database via the $(b,sqlite3) tool."; ]
  in
  Cmd.make (Cmd.info "sql" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ args =
    let doc = "Arguments for the sqlite3 tool." and docv = "ARG" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv)
  in
  sql ~config ~args

let cmd =
  let doc = "Manage the application database" in
  let man = [
    `S Manpage.s_description;
    `P "The $(cmd) command manages the application database."; ]
  in
  Cmd.group (Cmd.info "db" ~doc ~man) @@
  [backup_cmd; changes_cmd; reset_cmd; restore_cmd; schema_cmd; sql_cmd]
