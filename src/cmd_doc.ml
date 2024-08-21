(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel


let test ~file conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  Ok Cli_kit.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let test_cmd =
  let doc = "Test" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) is used for testing purposes."; ]
  in
  Cli_kit.cmd_with_conf "test" ~doc ~man @@
  let+ file =
    let doc = "$(docv) is the backup file to restore." in
    let docv = "BACKUP.sqlite3" in
    Arg.(required & pos 0 (some Cli_kit.fpath) None & info [] ~doc ~docv)
  in
  test ~file

let cmd =
  let doc = "Operations on reference documents" in
  Cli_kit.cmd_group "doc" ~doc @@
  [test_cmd]
