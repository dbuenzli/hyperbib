(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

(* BibTeX export *)

let bibtex ~outf conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  Hyperbib_conf.with_db_transaction conf `Deferred @@ fun db ->
  let* b = Bibliography.get () in
  let now = Ptime_clock.now () in
  let only_public = Rel_query.Bool.true' in
  let refs = Reference.list ~only_public in
  let* refs = Reference.render_data ~only_public refs db |> Db.string_error in
  let* bib = Export.bibtex_of_refs ~now b refs in
  let* () = Os.File.write ~force:true ~make_path:false outf bib in
  Ok Hyperbib_cli.Exit.ok

(* HTML export *)

let page_gen ~file_browsable bibliography =
  let now = Ptime_clock.now () in
  let uf =
    let init = Kurl.Fmt.empty ~use_exts:file_browsable ~root:[""] () in
    Service_tree.url_fmt ~init
  in
  let auth_ui = None and user_view = None and private_data = false in
  let testing = false in
  Page.Gen.v ~now bibliography uf ~auth_ui ~user_view ~private_data ~testing

let html ~dest ~file_browsable conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  Hyperbib_conf.with_db_transaction conf `Deferred @@ fun db ->
  let* b = Bibliography.get () in
  let page_gen = page_gen ~file_browsable b in
  let* () = Export.static_html ~inside_dir:dest conf db page_gen in
  Ok Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let bibtex =
  let doc = "BibTeX export" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command exports the public bibliography as a \
        BibTeX file." ]
  in
  Hyperbib_cli.cmd_with_conf "bibtex" ~doc ~man @@
  let+ outf =
    let doc = "Write BibTeX file to $(docv). Use $(b,-) for $(stdout)." in
    Arg.(value & opt More_cli.filepath Fpath.dash & info ["o"] ~doc)
  in
  bibtex ~outf

let html =
  let doc = "Static HTML export" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command extracts the public bibliography as a set of \
        static HTML files.";
    `P "Use option $(b,--file-browsable) if you want the extract to be \
        browsable via the $(b,file://) protocol. Do not use this option if you \
        are turning an dynamic service to a static one as it will break \
        external links to it (you will have to configure your webserver \
        to append $(b,.html) to requests)." ]
  in
  Hyperbib_cli.cmd_with_conf "html" ~doc ~man @@
  let+ dest =
    let doc = "Output directory." and docv = "HTML_DIR" in
    Arg.(required & pos 1 (some More_cli.dirpath) None & info [] ~doc ~docv)
  and+ file_browsable =
    let doc = "Ensure the HTML can be browsed via the $(b,file://) protocol." in
    Arg.(value & flag & info ["file-browsable"] ~doc)
  in
  html ~dest ~file_browsable

let cmd =
  let doc = "Export data from the database" in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command exports data in the database."  ]
  in
  Hyperbib_cli.cmd_group "export" ~doc ~man [bibtex; html]
