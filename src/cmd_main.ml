(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Cmdliner

let cmds =
  [ Cmd_user.cmd; Cmd_db.cmd; Cmd_html.cmd; Cmd_import.cmd;
    Cmd_serve.cmd; ]

let hyperbib =
  let doc = "Annotates bibliographies" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  let info = Cmd.info "hyperbib" ~version:Stamp.version ~doc ~exits in
  Cmd.group info ~default cmds

let main () =
  Os.Exit.exit @@
  Log.time (fun _ m -> m "total time hyperbib %s" Stamp.version) @@ fun () ->
  match Cmd.eval_value' hyperbib with `Ok e -> e | `Exit c -> Os.Exit.code c

let () = if !Sys.interactive then () else main ()
