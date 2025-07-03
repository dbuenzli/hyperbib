(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

module Exit = struct
  open Cmdliner
  let ok = Os.Exit.code Cmd.Exit.ok
  let user_exists = Os.Exit.Code 1
  let conf_error = Os.Exit.Code 122
  let some_error = Os.Exit.Code Cmd.Exit.some_error
  module Info = struct
    let e c doc = Cmdliner.Cmd.Exit.info (Os.Exit.get_code c) ~doc
    let user_exists = e user_exists "on adding an existing user."
    let conf_error = e conf_error "on configuration error."
    let base_cmd = user_exists :: conf_error :: Cmd.Exit.defaults
  end
end

open Cmdliner
open Cmdliner.Term.Syntax

let common_man = []
let docs = Manpage.s_common_options

let conf =
  Term.term_result' @@
  let+ () = More_cli.set_log_level ()
  and+ app_dir =
    let doc = "Application directory." in
    let absent = "current working directory" in
    let env = Cmd.Env.info "HYPERBIB_APP_DIR" in
    Arg.(value & opt (some ~none:"." More_cli.dirpath) None &
         info ["a"; "app-dir"] ~doc ~docs ~env ~absent)
  in
  Hyperbib_conf.with_cli ~app_dir

let cmd ?doc ?(man = []) ?(exits = []) name term =
  let man = [`Blocks man; `Blocks common_man] in
  let exits = List.append exits Exit.Info.base_cmd in
  Cmd.make (Cmd.info name ~exits ?doc ~man) term

let cmd_with_conf ?doc ?man ?exits name term =
  cmd ?doc ?man ?exits name Term.(term $ conf)

let cmd_group ?doc ?(man = []) ?(exits = []) ?default name cmds =
  let man = [`Blocks man; `Blocks common_man] in
  let exits = List.append exits Exit.Info.base_cmd in
  Cmd.group (Cmd.info name ~exits ?doc ~man) ?default cmds
