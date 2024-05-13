(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let import_legacy conf reset =
  (* FIXME remove that *)
  Log.if_error ~use:Hyperbib_app.Exit.some_error @@
  match reset with
  | false ->
      Fmt.error "Cannot import without clearing the database use %a option"
        Fmt.code "--reset"
  | true ->
      let db_file = Hyperbib_app.Conf.db_file conf in
      let file_error e = Fmt.str "%a: %s" Fpath.pp_unquoted db_file e in
      Result.map_error file_error @@ Result.join @@ Db.string_error @@
      Db.with_open ~foreign_keys:false db_file @@ fun db ->
      let* () = Db.clear db |> Db.string_error in
      let* () = Db.ensure_schema Schema.v db in
      let* () = Import.legacy db conf |> Db.string_error |> Result.join in
      Ok Hyperbib_app.Exit.ok

let db conf action reset = match action with
| `Legacy -> import_legacy conf reset

(* Command line interface *)

open Cmdliner

let doc = "Import data in the database"
let exits = Hyperbib_app.Exit.Info.base_cmd
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
    Term.(const db $ Hyperbib_app.Cli.conf $ action $ reset)
