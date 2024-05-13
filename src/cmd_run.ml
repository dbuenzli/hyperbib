(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let reference_dois =
  let open Rel_query.Syntax in
  let* r = Bag.table Reference.table in
  let doi = r #. Reference.doi' in
  Bag.where Text.(not (doi = empty)) (Bag.yield doi)

let check_dois () conf : B0_std.Os.Exit.t =
  Log.if_error ~use:Hyperbib_app.Exit.some_error @@
  let db_file = Hyperbib_app.Conf.db_file conf in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Result.join @@ Db.with_transaction `Deferred db @@ fun db ->
  let stmt = Rel_query.Sql.of_bag Rel.Row.(t1 (text "doi")) reference_dois in
  let* () = Db.fold db stmt (fun id () -> print_endline id) () in
  Ok Hyperbib_app.Exit.ok

let test () conf =
  Log.if_error ~use:Hyperbib_app.Exit.some_error @@
  let db_file = Hyperbib_app.Conf.db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction `Deferred db @@ fun db ->
  Ok Hyperbib_app.Exit.ok


(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let check_dois_cmd =
  let doc = "Check DOIs" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) command is used for checking DOIs."; ]
  in
  Hyperbib_app.Cli.cmd_with_conf "check-dois" ~doc ~man @@
  let+ () = Term.const () in
  check_dois ()

let test_cmd =
  let doc = "Test" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) is used for testing purposes."; ]
  in
  Hyperbib_app.Cli.cmd_with_conf "test" ~doc ~man @@
  let+ () = Term.const () in
  test ()

let cmd =
  let doc = "Run maintenance tasks" in
  Hyperbib_app.Cli.cmd_group "run" ~doc @@
  [test_cmd; check_dois_cmd]
