(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let diagram () =
  Format.printf "%a@." (Rel_kit.Schema_diagram.pp_dot ()) Schema.tables;
  Hyperbib.Exit.ok

let schema () =
  let schema = Sql.create_schema Schema.tables in
  let stmts = [schema] in
  Log.app (fun m -> m "@[<v>%a@]" (Fmt.list Sql.Stmt.pp_src) stmts);
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

let diagram_cmd =
  let doc = "Output database schema diagram" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the database schema $(b,.dot) file format. Pipe \
        to $(b,dot -Tsvg) to generate an SVG file."; ]
  in
  Cmd.v (Cmd.info "diagram" ~doc ~exits ~man) Term.(const diagram $ const ())

let schema_cmd =
  let doc = "Output database SQL schema" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the datbase schema in the SQL data definition \
        language."]
  in
  Cmd.v (Cmd.info "schema" ~doc ~exits ~man) Term.(const schema $ const ())

let sql_cmd =
  let doc = "SQL prompt on the database" in
  let exits = Hyperbib.Exit.Info.base_cmd in
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
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command manages the application database."; ]
  in
  Cmd.group (Cmd.info "db" ~doc ~exits ~man) [diagram_cmd; schema_cmd; sql_cmd]


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
