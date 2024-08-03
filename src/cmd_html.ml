(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
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

let html conf inside_dir file_browsable =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let* () = Hyperbib.Conf.ensure_data_dir conf in
  let db_file = Hyperbib.Conf.db_file conf in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Result.join @@ Result.join @@ Result.map Db.string_error @@
  Db.with_open_schema Schema.v db_file @@ fun db ->
  Db.with_transaction `Deferred db @@ fun db ->
  let* b = Bibliography.get () in
  let page_gen = page_gen ~file_browsable b in
  let* () = Export.static_html ~inside_dir conf db page_gen in
  Ok Hyperbib.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Static HTML generation"
let exits = Hyperbib.Exit.Info.base_cmd
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command extracts the public bibliography as a set of \
      static HTML files.";
  `P "Use option $(b,--file-browsable) if you want the extract to be \
      browsable via the $(b,file://) protocol. Do not use this option if you \
      are turning an dynamic service to a static one as it will break \
      external links to it (you will have to configure your webserver \
      to append $(b,.html) to requests)." ]

let dest =
  let doc = "Output directory." and docv = "HTML_DIR" in
  Arg.(required & pos 1 (some Hyperbib.Cli.fpath) None & info [] ~doc ~docv)

let file_browsable =
  let doc = "Ensure the HTML can be browsed via the $(b,file://) protocol." in
  Arg.(value & flag & info ["file-browsable"] ~doc)

let cmd =
  Cmd.v (Cmd.info "html" ~doc ~exits ~man)
    Term.(const html $ Hyperbib.Cli.conf $ dest $ file_browsable)
