(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let output_config ~config =
  Fmt.pr "@[%a@]@." Hyperbib_config.pp config;
  Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Output the configuration of $(tool)" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) output the configuration of $(tool) on $(b,stdout)" ]
  in
  Cmd.make (Cmd.info "config" ~doc ~man) @@
  let+ config = Hyperbib_cli.config in
  output_config ~config
