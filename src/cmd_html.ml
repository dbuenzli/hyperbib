(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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


(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

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
