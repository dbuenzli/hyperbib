(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

let check_dois () conf =
  let ref_dois = Row.(t2 Reference.id' (text "doi")) in
  let reference_dois =
    (* FIXME rel this is too much boilerplate ! :-) *)
    let open Rel_query.Syntax in
    let* r = Bag.table Reference.table in
    let id = r #. Reference.id' in
    let doi = r #. Reference.doi' in
    Bag.where Bool.true'
      (Bag.yield (Bag.row (fun id doi -> id, doi) $ id $ doi))
  in
  let reference_dois_stmts = Rel_query.Sql.of_bag ref_dois reference_dois
  in
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  Cli_kit.with_db_transaction conf `Deferred @@ fun db ->
  Log.app (fun m -> m "Checking DOIs in the %a table"
              Fmt.code (Table.name Reference.table));
  let check (id, doi) () =
    if doi = ""
    then
      Log.err (fun m -> m "reference %d: Invalid empty DOI, should be NULL" id)
    else match Doi.of_string doi with
    | Error e ->
        if doi = "" then
          Log.err (fun m -> m "reference %d: DOI %S: %s" id doi e)
    | Ok doi' ->
        let doi' = Doi.to_string doi' in
        if doi' = doi then () else
        Log.warn (fun m ->
            m "reference %d: DOI %S not normalized (%S)" id doi doi')
  in
  let* () = Db.fold db
      (Db.show_plan db
         (Db.show_sql reference_dois_stmts)) check () |> Db.string_error in
  Ok Cli_kit.Exit.ok

let test () conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  Cli_kit.with_db_transaction conf `Deferred @@ fun db ->
  let stmt = "-- ha\n#pragma nop;" in
  let* () = Db.exec_sql db stmt |> Db.string_error in
  let stmt = Rel_sql.Stmt.(func stmt unit) in
  let* () = Result.map_error Db.error_rc_message (Db.exec db stmt) in
  Log.app (fun m -> m "Ok");
  Ok Cli_kit.Exit.ok


(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let check_dois_cmd =
  let doc = "Check DOIs" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) command is used for checking DOIs in the database"; ]
  in
  Cli_kit.cmd_with_conf "check-dois" ~doc ~man @@
  let+ () = Term.const () in
  check_dois ()

let test_cmd =
  let doc = "Test" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) is used for testing purposes."; ]
  in
  Cli_kit.cmd_with_conf "test" ~doc ~man @@
  let+ () = Term.const () in
  test ()

let cmd =
  let doc = "Run maintenance tasks" in
  Cli_kit.cmd_group "run" ~doc @@
  [test_cmd; check_dois_cmd]
