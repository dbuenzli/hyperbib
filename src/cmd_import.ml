(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let import_legacy data_conf reset =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  match reset with
  | false ->
      Fmt.error "Cannot import without clearing the database use %a option"
        Fmt.(code string) "--reset"
  | true ->
      let db_file = Hyperbib.Data_conf.db_file data_conf in
      let file_error e = Fmt.str "%a: %s" Fpath.pp_unquoted db_file e in
      Result.join @@ Result.map_error file_error @@ Db.error_string @@
      let foreign_keys = false in
      let* db = Db.open' ~foreign_keys (Hyperbib.Data_conf.db_file data_conf) in
      let* () = Db.setup ~schema:Schema.tables ~drop_if_exists:true db in
      let* ret = Import.legacy db data_conf in
      Ok (Result.map (Fun.const Hyperbib.Exit.ok) ret)

let db conf action data_conf reset = match action with
| `Legacy -> import_legacy data_conf reset

(* Command line interface *)

open Cmdliner

let doc = "Import data in the database"
let exits = Hyperbib.Exit.Info.base_cmd
let man = [
  `S Manpage.s_description;
  `P "The $(tname) imports data in the database."; ]

let reset =
  let doc = "Resets the database. This deletes all existing data." in
  Arg.(value & flag & info ["reset"] ~doc)

let action =
  let action = [ "legacy", `Legacy; ] in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let cmd =
  Cmd.v (Cmd.info "import" ~doc ~exits ~man)
    Term.(const db $ Hyperbib.Cli.conf $ action $
          Hyperbib.Cli.data_conf ~pos:1 $ reset)


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
