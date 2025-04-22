(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let doi ~reset ~dois ~files ~dry_run conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let files = match dois, files with [], [] -> [Fpath.dash] | _ -> files in
  let dois_of_file file acc =
    Log.info (fun m -> m "Extracting DOIs from %a" Fpath.pp file);
    let* contents = Os.File.read file in
    Ok (Doi.fold_text_scrape Doi.Set.add contents acc)
  in
  Log.info (fun m -> m "Gathering DOIs");
  let dois = Doi.Set.of_list dois in
  let* dois = List.fold_stop_on_error dois_of_file files dois in
  match dry_run with
  | true ->
      let dois = Doi.Set.to_list dois in
      let dois = List.map Doi.to_string dois in
      Log.stdout (fun m -> m "@[<v>%a@]" (Fmt.iter List.iter Fmt.string) dois);
      Ok Hyperbib_cli.Exit.ok
  | false ->
      failwith "TODO"
  (*
  match reset with
  | false ->
      Fmt.error "Cannot import without clearing the database use %a option"
        Fmt.code "--reset"
  | true ->
      let db_file = Cli_kit.Conf.db_file conf in
      let file_error e = Fmt.str "%a: %s" Fpath.pp_unquoted db_file e in
      Result.map_error file_error @@ Result.join @@ Db.string_error @@
      Db.with_open ~foreign_keys:false db_file @@ fun db ->
      let* () = Db.clear db |> Db.string_error in
      let* () = Db.ensure_schema Schema.v db in
      let* () = Import.legacy db conf |> Db.string_error |> Result.join in
      Ok Hyperbib_cli.Exit.ok *)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let reset =
  let doc = "Resets the database. This deletes all existing data." in
  Arg.(value & flag & info ["reset"] ~doc)

let dry_run =
  let doc =
    "Do not perform import. Output information about what would be imported."
  in
  Arg.(value & flag & info ["dry-run"] ~doc)

let doi_cmd =
  let doc = "Import bibliographic references by DOI." in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command imports data in the database by DOI. \
        DOIs can be specified explicitely with $(b,-d) option or \
        be extracted from text files. For example:";
    `Pre "$(iname) $(b,--dry-run -d https://doi.org/10.2307/1968337)"; `Noblank;
    `Pre "$(iname) $(b,--dry-run -d doi:10.2307/1968337)"; `Noblank;
    `Pre "$(iname) $(b,--dry-run -d 10.2307/1968337)"; `Noblank;
    `Pre "$(iname) $(b,--dry-run README.md dois.csv)"; `Noblank;
    `Pre "$(b,pdftotext paper.pdf -) | $(iname) $(b,--dry-run)"; `Noblank;
    `P "Note that it won't work well if your text format needs to \
        escape the DOIs, e.g. on BibTeX files." ]
  in
  Hyperbib_cli.cmd_with_conf "doi" ~doc ~man @@
  let+ reset and+ dry_run
  and+ dois =
    let doc = "Import reference $(docv). Repeatable." in
    let doi_conv = Arg.conv' Doi.(of_string, pp) ~docv:"DOI" in
    Arg.(value & opt_all doi_conv [] & info ["d"; "doi"] ~doc ~docv:"DOI")
  and+ files =
    let doc = "Extract DOIs from (UTF-8 or ASCII compatible) text $(docv)."in
    let docv = "FILE" in
    let absent = "$(b,stdin) if no DOI is specified" in
    let path_conv = Arg.conv' Fpath.(of_string, pp) in
    Arg.(value & pos_all path_conv [] & info [] ~doc ~docv ~absent)
  in
  doi ~reset ~dois ~files ~dry_run

let cmd =
  let doc = "Bulk import data in the database" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command imports data in the database."  ]
  in
  Hyperbib_cli.cmd_group "import" ~doc ~man @@
  [doi_cmd]
