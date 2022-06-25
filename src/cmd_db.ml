(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let schema conf data_conf which format =
  let output_schema ~format s = match format with
  | `Dot rankdir ->
      Log.app (fun m -> m "@[%a@]" (Rel.Schema.pp_dot ~rankdir) s);
  | `Sqlite3 ->
      let stmts = Rel_sql.create_schema Db.dialect s in
      Log.app (fun m -> m "@[<v>%a@]" (Fmt.list Rel_sql.Stmt.pp_src) stmts);
  | `Ocaml kind ->
      Log.app (fun m -> m "@[%a@]" (Rel.Schema.pp_ocaml kind) s)
  in
  match which with
  | `Live ->
      let use = Hyperbib.Exit.some_error in
      Log.if_error ~use @@ Db.string_error @@ Result.join @@
      Db.with_open (Hyperbib.Data_conf.db_file data_conf) @@ fun db ->
      let* live, issues = Db.schema db in
      output_schema ~format live;
      List.iter (fun i -> Log.warn (fun m -> m "%a" Fmt.lines i)) issues;
      Ok Hyperbib.Exit.ok
  | `App ->
      output_schema ~format Schema.v;
      Hyperbib.Exit.ok

let sql conf data_conf args =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let db_file = Hyperbib.Data_conf.db_file data_conf in
  let args = match List.rev args with
  | [] -> Cmd.path db_file
  | a :: _ when String.length a > 1 && a.[0] = '-' (* is an option *) ->
      Cmd.(list args %% path db_file)
  | sql :: ropts -> Cmd.(list (List.rev ropts) %% path db_file % sql)
  in
  let* sqlite3 = Os.Cmd.get_tool (Fpath.v "sqlite3") in
  let exec = Os.Exit.exec sqlite3 Cmd.(path sqlite3 %% args) in
  Os.Exit.exit exec

(* Command line interface *)

open Cmdliner

let exits = Hyperbib.Exit.Info.base_cmd

let schema_cmd =
  let doc = "Output the app or live database schema" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the app or live database schema in various formats."]
  in
  let which =
    let e = ["app", Some `App; "live", Some `Live] in
    let doc = Fmt.str
        "Schema to output. Must be %s. $(b,app) is the schema assumed by \
         the software. $(b,live) is the schema of the database."
        (Arg.doc_alts_enum e)
    in
    let docv = "WHICH" in
    Arg.(required & pos 0 (Arg.enum e) None & info [] ~doc ~docv)
  in
  Cmd.v (Cmd.info "schema" ~doc ~exits ~man)
    Term.(const schema $ Hyperbib.Cli.conf $ Hyperbib.Cli.data_conf $
          which $ Rel_cli.schema_format ~default:`Sqlite3 ())

let sql_cmd =
  let doc = "SQL prompt on the database" in
  let man = [
     `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]… $(b,--) $(i,OPTION)… [$(i,SQL)]";
    `S Manpage.s_description;
    `P "$(tname) gets you an interactive SQL prompt to interact with \
        the database via the $(b,sqlite3) tool."; ]
  in
  let args =
    let doc = "Arguments for the sqlite3 tool." and docv = "ARG" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv)
  in
  Cmd.v (Cmd.info "sql" ~doc ~exits ~man)
    Term.(const sql $ Hyperbib.Cli.conf $ Hyperbib.Cli.data_conf $ args)

let cmd =
  let doc = "Manage the database of the application." in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command manages the application database."; ]
  in
  Cmd.group (Cmd.info "db" ~doc ~exits ~man) [schema_cmd; sql_cmd]


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
