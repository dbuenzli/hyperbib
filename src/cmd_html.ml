(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

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
  Log.if_error ~use:Hyperbib_app.Exit.some_error @@
  let db_file = Hyperbib_app.Conf.db_file conf in
  let* () = Db.ensure_db_path db_file in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction `Deferred db @@ fun db ->
  let* b = Bibliography.get () in
  let page_gen = page_gen ~file_browsable b in
  let* () = Export.static_html ~inside_dir:dest conf db page_gen in
  Ok Hyperbib_app.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let dest =
  let doc = "Output directory." and docv = "HTML_DIR" in
  Arg.(required & pos 1 (some Hyperbib_app.Cli.fpath) None & info [] ~doc ~docv)

let file_browsable =
  let doc = "Ensure the HTML can be browsed via the $(b,file://) protocol." in
  Arg.(value & flag & info ["file-browsable"] ~doc)

let cmd =
  let doc = "Static HTML generation" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command extracts the public bibliography as a set of \
        static HTML files.";
    `P "Use option $(b,--file-browsable) if you want the extract to be \
        browsable via the $(b,file://) protocol. Do not use this option if you \
        are turning an dynamic service to a static one as it will break \
        external links to it (you will have to configure your webserver \
        to append $(b,.html) to requests)." ]
  in
  Hyperbib_app.Cli.cmd_with_conf "html" ~doc ~man @@
  let+ dest and+ file_browsable in
  html ~dest ~file_browsable
