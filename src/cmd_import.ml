(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let doi ~reset conf =
  Log.if_error ~use:Hyperbib_app.Exit.some_error @@
  Fmt.error "TODO"
  (*
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
      Ok Hyperbib_app.Exit.ok *)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let reset =
  let doc = "Resets the database. This deletes all existing data." in
  Arg.(value & flag & info ["reset"] ~doc)

let doi_cmd =
  let doc = "Import bibliographic references by DOI." in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command imports data in the database."; ]
  in
  Hyperbib_app.Cli.cmd_with_conf "doi" ~doc ~man @@
  let+ reset in
  doi ~reset

let cmd =
  let doc = "Bulk import data in the database" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command imports data in the database."; ]
  in
  Hyperbib_app.Cli.cmd_group "import" ~doc ~man @@
  [doi_cmd]
