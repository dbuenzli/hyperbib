(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let clear ~config ~only_dois =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let dir =
    if only_dois
    then Hyperbib_config.doi_cache_dir config
    else Hyperbib_config.cache_dir config
  in
  let* _existed = Os.Dir.delete ~recurse:true dir in
  Ok Hyperbib_cli.Exit.ok

let path ~config =
  Fmt.pr "@[%a@]@." Fpath.pp (Hyperbib_config.cache_dir config);
  Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let clear_cmd =
  let doc = "Clear the cache directory" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) deletes cache directories."
  ]
  in
  Cmd.make (Cmd.info "clear" ~doc ~man) @@
  let+ config = Hyperbib_cli.config
  and+ only_dois =
    let doc = "Only delete the DOI request cache" in
    Arg.(value & flag & info ["only-dois"] ~doc)
  in
  clear ~config ~only_dois

let path_cmd =
  let doc = "Output the path to the cache directory" in
  Cmd.make (Cmd.info "path" ~doc) @@
  let+ config = Hyperbib_cli.config in
  path ~config


let cmd =
  let doc = "Operate on the cache directory" in
  Cmd.group (Cmd.info "cache" ~doc) @@
  [clear_cmd; path_cmd]
