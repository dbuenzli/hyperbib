(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open B00_http

let get_ref httpr doi_meta ~src bib = match Bibtex.doi bib with
| Some doi ->
    (*    Result.map_error (fun err -> Some doi, err) @@ *)
    Log.if_error ~use:None @@
    let* json = Crossref.for_doi httpr ~cache:doi_meta doi in
    let* ref = Legacy.ref_of_bib_and_doi_meta doi bib json in
    Ok (Some ref)
| None -> None
    (* Error (None, Fmt.str "%a: Missing DOI" Fpath.pp_unquoted src) *)

let bib_to_refs httpr doi_meta bibfile =
  let* contents = Os.File.read bibfile in
  let* entries = Bibtex.of_string' ~file:bibfile contents in
  Ok (List.filter_map (get_ref httpr doi_meta ~src:bibfile) entries)

(* From bib files. *)
let bibs_to_refs httpr doi_meta bib_data data_tables =
  Result.bind (Os.Dir.(fold_files ~recurse:true path_list bib_data []))
  @@ fun files ->
  let files = List.filter (Fpath.has_ext ".bib") files in
  let rec loop acc = function
  | [] -> List.rev acc
  | bibfile :: fs ->
      let acc =
        Log.if_error ~use:acc @@
        Result.bind (bib_to_refs httpr doi_meta bibfile) @@ fun refs ->
        Ok (refs :: acc)
      in
      loop acc fs
  in
  Ok (List.concat @@ loop [] files)

(* From the tables. *)
let bibs_to_refs httpr doi_meta data_tables =
  let refs_table = Fpath.(data_tables / "refs.json") in
  let* bibs = Legacy.Data_tables.refs_to_bibs ~file:refs_table in
  Ok (List.filter_map (get_ref httpr doi_meta ~src:Fpath.dash) bibs)

let log_ref r =
  let date = match Legacy.Reference'.date r with
  | None -> "????"
  | Some (y, _) -> string_of_int y
  in
  let title = match Legacy.Reference'.title r with
  | None -> "S.N."
  | Some title -> title
  in
  let ctitle = Option.value ~default:"" (Legacy.Reference'.container_title r) in
  let doi = match Legacy.Reference'.doi r with
  | None -> "????" | Some doi -> doi
  in
  Log.app (fun m -> m "@[%s %s. %s doi:%s@]" date title ctitle doi)

let copy_assets html_dir assets_dir =
  let paths = Os.Dir.fold ~recurse:false Os.Dir.path_list assets_dir [] in
  let paths = Log.if_error ~use:[] paths in
  let copy src =
    let dst = Fpath.reroot ~root:assets_dir ~dst:html_dir src in
    Log.if_error ~use:() @@
    Result.bind (Os.Path.delete ~recurse:true dst) @@ fun _ ->
    Os.Path.copy ~make_path:true ~recurse:true ~src dst
  in
  List.iter copy paths

let gen
    conf data_conf curl _bib_data doi_meta data_tables html_dir assets_dir
    assets_only csv _sem_urls
  =
  Log.if_error ~header:"" ~use:Hyperbib.Exit.some_error @@
  let* httpr = Httpr.get_curl ~curl () in
  let* refs = bibs_to_refs (Some httpr) doi_meta (* bib_data *) data_tables in
  let subjs_table = Fpath.(data_tables / "subjects.json") in
  let* subjs = Legacy.Data_tables.subjs_to_subjects ~file:subjs_table in
  match csv with
  | true ->
      let* s = Legacy.gen_csv refs in
      Log.app (fun m -> m "%s" s);
      Ok Hyperbib.Exit.ok
  | false ->
      copy_assets html_dir assets_dir;
      if assets_only then Ok Hyperbib.Exit.ok else
      let subj_db =
        let db =
          List.fold_left Legacy.Subject'.Db.add Legacy.Subject'.Db.empty subjs
        in
        Legacy.Subject'.Db.make_hierarchy db
      in
      let refdb = Legacy.Refdb.empty subj_db in
      let refdb = List.fold_left Legacy.Refdb.add refdb refs in
      let* bib_conf = Bibliography.get () in
      let now = Ptime_clock.now () in
      let rf =
        let init = Kurl.Fmt.empty ~root:[""] () in
        Service.url_fmt ~init
      in
      let auth_ui = None and user_view = None and private_data = false in
      let testing = false in
      let g =
        Page.Gen.v ~now bib_conf rf ~auth_ui ~user_view ~private_data ~testing
      in
      let* () = Legacy_html.gen_html ~dir:html_dir g refdb in
      Ok Hyperbib.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Legacy HTML generation"
let sdocs = Manpage.s_common_options
let exits = Hyperbib.Exit.Info.base_cmd
let man_xrefs = [ `Main ]

let cmd =
  let bib_data =
    let doc = "The directory with BibTeX files." in
    let default = Fpath.(v "data/bib") in
    Arg.(value & pos 0 B00_cli.fpath default & info [] ~docv:"DIR" ~doc)
  in
  let doi_meta =
    let doc = "The directory for DOI derived metadata." in
    let default = Fpath.(v "app/dois") in
    Arg.(value & opt B00_cli.fpath default &
          info ["data-dir"] ~doc ~docv:"DIR")
  in
  let data_tables =
    let doc = "The directory with ad-hoc tables." in
    let default = Fpath.(v "app/tables") in
    Arg.(value & opt B00_cli.fpath default &
          info ["data-tables"] ~doc ~docv:"DIR")
  in
  let html_dir =
    let doc = "The HTML directory." in
    let default = Fpath.(v "app/static") in
    Arg.(value & opt B00_cli.fpath default &
          info ["html-dir"] ~doc ~docv:"DIR")
  in
  let assets_dir =
    let doc = "The assets directory" in
    let default = Fpath.(v "src-front") in
    Arg.(value & opt B00_cli.fpath default &
         info ["assets-dir"] ~doc ~docv:"DIR")
  in
  let assets_only =
    let doc = "Only regenerate assets" in
    Arg.(value & flag & info ["assets-only"] ~doc)
  in
  let sem_urls =
    let doc = "Generate semantic URLs" in
    Arg.(value & flag & info ["semantic-urls"] ~doc)
  in
  let csv =
    let doc = "Generate db-source csv" in
    Arg.(value & flag & info ["csv"] ~doc)
  in
  Term.(const gen $
        Hyperbib.Cli.conf $ Hyperbib.Cli.data_conf ~pos:0 $
        Httpr.curl ~docs:sdocs () $
        bib_data $ doi_meta $ data_tables $ html_dir $ assets_dir $
        assets_only $ csv $ sem_urls),
  Term.info "legacy" ~doc ~sdocs ~exits ~man_xrefs

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

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
