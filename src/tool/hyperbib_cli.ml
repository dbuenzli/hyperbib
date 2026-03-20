(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

module Exit = struct
  open Cmdliner
  let ok = Os.Exit.code Cmd.Exit.ok
  let user_exists = Os.Exit.Code 1
  let some_error = Os.Exit.Code Cmd.Exit.some_error
  module Info = struct
    let e c doc = Cmdliner.Cmd.Exit.info (Os.Exit.get_code c) ~doc
    let user_exists = e user_exists "on adding an existing user."
    let user_cmd = user_exists :: Cmd.Exit.defaults
  end
end

open Cmdliner
open Cmdliner.Term.Syntax

let config =
  Term.term_result' @@
  let docs = Manpage.s_common_options in
  let+ () = More_cli.set_no_color ()
  and+ () = More_cli.set_log_level ()
  and+ app_dir =
    let doc = "Application directory." in
    let absent = "current working directory" in
    let env = Cmd.Env.info "HYPERBIB_APP_DIR" in
    Arg.(value & opt (some ~none:"." More_cli.dirpath) None &
         info ["a"; "app-dir"] ~doc ~docs ~env ~absent)
  in
  Hyperbib_config.discover ~app_dir
