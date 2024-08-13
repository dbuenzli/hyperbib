(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Cmdliner

let cmds =
  [ Cmd_user.cmd; Cmd_db.cmd; Cmd_export.cmd; Cmd_import.cmd;
    Cmd_serve.cmd; Cmd_run.cmd ]

let hyperbib =
  let doc = "Annotates bibliographies" in
  let exits = Cli_kit.Exit.Info.base_cmd in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  let info = Cmd.info "hyperbib" ~version:Stamp.version ~doc ~exits in
  Cmd.group info ~default cmds

let main () =
  Log.time (fun _ m -> m "total time hyperbib %s" Stamp.version) @@ fun () ->
  match Cmd.eval_value' hyperbib with `Ok e -> e | `Exit c -> Os.Exit.code c

let () = if !Sys.interactive then () else Os.Exit.exit (main ())
