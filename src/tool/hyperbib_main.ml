(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Cmdliner

let version = Stamp.version

let cmd =
  let doc = "Annotates bibliographies" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) is a web application to annotate bibliographies."
  ]
  in
  Cmd.group (Cmd.info "hyperbib" ~version:Stamp.version ~doc ~man) @@
  [ Cmd_db.cmd; Cmd_config.cmd; Cmd_doc.cmd; Cmd_export.cmd;
    Cmd_import.cmd; Cmd_run.cmd; Cmd_serve.cmd; Cmd_user.cmd; ]

let main () =
  Log.time (fun _ m -> m "total time hyperbib %s" version) @@ fun () ->
  match Cmd.eval_value' cmd with `Ok e -> e | `Exit c -> Os.Exit.code c

let () = if !Sys.interactive then () else Os.Exit.exit (main ())
