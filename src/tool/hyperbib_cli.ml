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
  and+ data_dir =
    let doc = "$(docv) is the data directory" in
    let absent =
      "$(tool) directory in cwd or $(b,HYPERBIB_DATA_DIR) or
        $(b,XDG_CACHE_HOME)/$(tool)"
    in
    let doc_envs =
      [Cmd.Env.info "HYPERBIB_DATA_DIR"; Cmd.Env.info "XDG_DATA_HOME"]
    in
    Arg.(value & opt (some More_cli.dirpath) None &
         info ["data-dir"] ~doc ~docs ~absent ~doc_envs)
  and+ cache_dir =
    let doc = "$(docv) is the cache directory" in
    let absent = "$(b,XDG_CACHE_HOME)/$(tool)" in
    let doc_envs = [Cmd.Env.info "XDG_CACHE_HOME"] in
    Arg.(value & opt (some More_cli.dirpath) None &
         info ["cache-dir"] ~absent ~doc ~docs ~doc_envs)
  in
  Hyperbib_config.discover ~data_dir ~cache_dir
