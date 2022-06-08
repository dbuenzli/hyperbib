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

(* Command line interface *)

open Cmdliner

let schema_cmd =
  let doc = "Output database SQL schema." in
  Cmd.v (Cmd.info "schema" ~doc) Term.(const schema $ const ())

let diagram_cmd =
  let doc = "Output database schema diagram" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the database schema $(b,.dot) file format.";
    `P "Pipe to $(v,dot -Tsvg) to generate an SVG file."; ]
  in
  Cmd.v (Cmd.info "diagram" ~doc ~exits ~man) Term.(const diagram $ const ())

let cmd =
  let doc = "Manage the database of the application." in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command manages the application database."; ]
  in
  Cmd.group (Cmd.info "db" ~doc ~exits ~man) [diagram_cmd; schema_cmd]


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
