(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

let pp_check ppf () = Fmt.(st [`Bg `White; `Fg `Black]) ppf " TEST "
let pp_pass ppf () = Fmt.(st [`Bg `Green; `Fg `Black]) ppf " PASS "
let pp_fail ppf () = Fmt.(st [`Bg `Red; `Fg `Black]) ppf " FAIL "

(* DOI check.

   Note the DOI check works because we use Doi.unsafe_of_string when
   get out DOIs from the db column and Doi.to_string is the
   identity. So we can recover unormalized dois despite them having
   gone through Doi.t *)

let check_reference_dois db ~repair =
  let reference_dois_stmt =
    let ref_dois = Row.(t2 Reference.id' Reference.doi') in
    let reference_dois =
      (* FIXME rel this is too much boilerplate ! :-) *)
      let open Rel_query.Syntax in
      let* r = Bag.table Reference.table in
      let id = r #. Reference.id' in
      let doi = r #. Reference.doi' in
      Bag.where Bool.true'
        (Bag.yield (Bag.row (fun id doi -> id, doi) $ id $ doi))
    in
    Rel_query.Sql.of_bag ref_dois reference_dois
  in
  let do_update ~id doi =
    let update_reference_doi ~id doi =
      let set = Col.[Value (Reference.doi', doi)] in
      let where = Col.[Value (Reference.id', id)] in
      Rel_sql.update Db.dialect Reference.table ~set ~where
    in
    let* () = Db.exec db (update_reference_doi ~id doi) in
    Ok (Log.stdout (fun m -> m "Reference %a: repaired" Reference.Id.pp id))
  in
  let check (id, doi) n = match doi with
  | None -> n
  | Some doi ->
      match Doi.to_string doi with
      | "" ->
          Log.err (fun m ->
              m "Reference %a: Invalid empty DOI, should be NULL"
                Reference.Id.pp id);
          (if not repair then () else
           (Log.if_error ~use:() @@ Db.string_error @@ do_update ~id None));
          n + 1
      | doi_raw ->
          match Doi.extract doi_raw with
          | None ->
              Log.err (fun m -> m "Reference %a: DOI %S: cannot extract a DOI"
                          Reference.Id.pp id doi_raw);
              n + 1
          | Some doi_reparsed ->
              let doi_reparsed_raw = Doi.to_string doi_reparsed in
              if String.equal doi_reparsed_raw doi_raw then n else begin
              Log.warn (fun m ->
                  m "Reference %a: DOI %S not normalized (%S)"
                    Reference.Id.pp id doi_raw doi_reparsed_raw);
              (if not repair then () else
               (Log.if_error ~use:() @@ Db.string_error @@
                do_update ~id (Some doi_reparsed)));
              n + 1
            end
  in
  Db.fold db reference_dois_stmt check 0 |> Db.string_error

 let check_cites_doi db ~repair =
   let cites_stmt =
     let cites =
       let open Rel_query.Syntax in
       let* r = Bag.table Reference.Cites.table in
       Bag.yield r
     in
     Rel_query.Sql.of_bag' Reference.Cites.table cites
   in
   let do_update oldr newr =
     let update oldr newr =
       let set =
         let ignore = Col.[Def Reference.Cites.reference'] in
         Bazaar.col_values (Table.row Reference.Cites.table) ~ignore newr
       in
       let where = (* XXX this is basically primary_key *)
         Bazaar.col_values (Table.row Reference.Cites.table) oldr
       in
       Rel_sql.update Db.dialect Reference.Cites.table ~set ~where
     in
     let* () = Db.exec db (update oldr newr) in
     Ok (Log.stdout
           (fun m -> m "Cites reference %a: repaired"
               Reference.Id.pp (Reference.Cites.reference newr)))
   in
   let check r n =
     let reference = Reference.Cites.reference r in
     let doi = Reference.Cites.doi r in
     let doi_raw = Doi.to_string doi in
     match
       (* It seems sometimes crossrefs gave us spaces *)
       Doi.extract (String.replace_all ~sub:" " ~by:"" doi_raw)
     with
     | None ->
         Log.err (fun m -> m "Cites reference %a: DOI %S: cannot extract a DOI"
                     Reference.Id.pp reference doi_raw);
         n + 1
     | Some doi_reparsed ->
         let doi_reparsed_raw = Doi.to_string doi_reparsed in
         if String.equal doi_reparsed_raw doi_raw then n else begin
           Log.warn begin fun m ->
             m "Cites reference %a: DOI %S not normalized (%S)"
               Reference.Id.pp reference doi_raw doi_reparsed_raw
           end;
           (if not repair then () else
            let newr = Reference.Cites.make ~reference ~doi:doi_reparsed in
            Log.if_error ~use:() @@ Db.string_error @@ do_update r newr);
           n + 1
         end
   in
   Db.fold db cites_stmt check 0 |> Db.string_error

let check_dois ~repair conf =
  let log_result ~repair n =
    if repair then () else
    Log.stdout (fun m -> m "%a@." (if n > 0 then pp_fail else pp_pass) ())
  in
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  Hyperbib_conf.with_db_transaction conf `Deferred @@ fun db ->
  Log.stdout (fun m -> m "%a DOIs in the %a table"
              pp_check () Fmt.code (Table.name Reference.Cites.table));
  let* n0 = check_cites_doi db ~repair in
  log_result ~repair n0;
  Log.stdout (fun m -> m "%a DOIs in the %a table"
              pp_check () Fmt.code (Table.name Reference.table));
  let* n1 = check_reference_dois db ~repair in
  log_result ~repair n1;
  if n0 + n1 = 0
  then Ok Hyperbib_cli.Exit.ok
  else Ok Hyperbib_cli.Exit.some_error

(* Check ORCIDs

   Note the ORCID check works because we use Orcid.unsafe_of_string when
   get out ORCIDs from the db column and Orcid.to_string is the
   identity. So we can recover un-normalized dois despite them having
   gone through Orcid.t *)

let check_person_orcids db ~repair =
  let person_orcid_stmt =
    let p_orcid = Row.t2 Person.id' Person.orcid' in
    let person_orcids =
      let open Rel_query.Syntax in
      let* r = Bag.table Person.table in
      let id = r #. Person.id' in
      let doi = r #. Person.orcid' in
      Bag.yield (Bag.row (fun id doi -> id, doi) $ id $ doi)
    in
    Rel_query.Sql.of_bag p_orcid person_orcids
  in
  let do_update ~id orcid =
    let update_person_orcid ~id orcid =
      let set = Col.[Value (Person.orcid', orcid)] in
      let where = Col.[Value (Person.id', id)] in
      Rel_sql.update Db.dialect Person.table ~set ~where
    in
    let* () = Db.exec db (update_person_orcid ~id orcid) in
    Ok (Log.stdout (fun m -> m "Person %a: repaired" Person.Id.pp id))
  in
  let check (id, orcid) n = match orcid with
  | None -> n
  | Some orcid ->
      match Orcid.to_string orcid with
      | "" ->
          Log.err (fun m ->
              m "Person %a: Invalid empty ORCID, should be NULL"
                Person.Id.pp id);
          (if not repair then () else
           (Log.if_error ~use:() @@ Db.string_error @@ do_update ~id None));
          n + 1
      | orcid_raw ->
          match Orcid.of_string orcid_raw with
          | Error e ->
              Log.err (fun m ->
                  m "Person %a: ORCID %S: %s" Person.Id.pp id orcid_raw e);
              n + 1
          | Ok orcid_reparsed ->
              let orcid_reparsed_raw = Orcid.to_string orcid_reparsed in
              if String.equal orcid_reparsed_raw orcid_raw then n else begin
                Log.warn (fun m ->
                    m "Person %a: DOI %S not normalized (%S)"
                      Person.Id.pp id orcid_raw orcid_reparsed_raw);
                (if not repair then () else
                 (Log.if_error ~use:() @@ Db.string_error @@
                  do_update ~id (Some orcid_reparsed)));
                n + 1
              end
  in
  Db.fold db person_orcid_stmt check 0 |> Db.string_error

let check_orcids ~repair conf =
  let log_result ~repair n =
    if repair then () else
    Log.stdout (fun m -> m "%a@." (if n > 0 then pp_fail else pp_pass) ())
  in
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  Hyperbib_conf.with_db_transaction conf `Deferred @@ fun db ->
  Log.stdout (fun m -> m "%a ORCIDs in the %a table"
              pp_check () Fmt.code (Table.name Person.table));
  let* n = check_person_orcids db ~repair in
  log_result ~repair n;
  if n = 0
  then Ok Hyperbib_cli.Exit.ok
  else Ok Hyperbib_cli.Exit.some_error

let test () conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  Result.join @@ Hyperbib_conf.with_db conf @@ fun db ->
  let open Rel_query.Syntax in
  Db.string_error @@
  let match' =
    Db.show_sql @@
    Person.match_stmt ~last:"Coulter" ~first:"Liese"
      ~orcid:None
  in
  let open Result.Syntax in
  let* persons = Db.list db match' in
  let pp_person = Row.value_pp (Table.row Person.table) in
  Log.stdout (fun m -> m "Persons: @[<v>%a@]" (Fmt.list pp_person) persons);
  Ok Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let check_dois_cmd =
  let doc = "Check DOIs" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) command is used for checking DOIs in the database."; ]
  in
  Hyperbib_cli.cmd_with_conf "check-dois" ~doc ~man @@
  let doc = "Repair warnings and errors that can be." in
  let+ repair = Arg.(value & flag & info ["repair"] ~doc) in
  check_dois ~repair

let check_orcids_cmd =
  let doc = "Check DOIs" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) command is used for checking ORCIDs in the database."; ]
  in
  Hyperbib_cli.cmd_with_conf "check-orcids" ~doc ~man @@
  let doc = "Repair warnings and errors that can be." in
  let+ repair = Arg.(value & flag & info ["repair"] ~doc) in
  check_orcids ~repair

let test_cmd =
  let doc = "Test" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) is used for testing purposes."; ]
  in
  Hyperbib_cli.cmd_with_conf "test" ~doc ~man @@
  let+ () = Term.const () in
  test ()

let cmd =
  let doc = "Run maintenance tasks" in
  Hyperbib_cli.cmd_group "run" ~doc @@
  [test_cmd; check_dois_cmd; check_orcids_cmd]
