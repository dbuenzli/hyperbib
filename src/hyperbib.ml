(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Std = struct

  (* Standard needs. *)

  module Char = B0_std.Char
  module String = B0_std.String
  module Result = B0_std.Result
  module Fmt = B0_std.Fmt
  module Log = B0_std.Log
  module List = B0_std.List
  module Fpath = B0_std.Fpath
  module Cmd = B0_std.Cmd
  module Os = B0_std.Os

  module Bazaar = struct
    let cp_dir_content
        ?dotfiles ?follow_symlinks ~recurse ~of_dir:src ~inside_dir:dst ()
      =
      let cp _ _ rel () =
        Log.if_error ~use:() @@
        let src = Fpath.(src // rel) and dst = Fpath.(dst // rel) in
        Os.File.copy ~force:true ~make_path:true src ~dst
      in
      Os.Dir.fold_files ?dotfiles ?follow_symlinks ~rel:true ~recurse cp src ()
  end

  (* Rel needs. *)

  module Bag = Rel_query.Bag

  (* Webs needs *)

  module Http = Webs.Http
  module Res = Webs_bazaar.Res
  module Kurl = Webs_bazaar.Kurl
  module At = Htmlit.At
  module El = Htmlit.El

end

open Std
open Result.Syntax

module Exit = struct
  let ok = Os.Exit.code 0
  let user_exists = Os.Exit.Code 1
  let conf_error = Os.Exit.Code 122
  let some_error = Os.Exit.Code 123
  module Info = struct
    let e c doc = Cmdliner.Cmd.Exit.info (Os.Exit.get_code c) ~doc
    let user_exists = e user_exists "on adding an exisiting user."
    let conf_error = e conf_error "on configuration error."
    let some_error = e some_error "on indiscriminate errors reported on stderr."
    let base_cmd = conf_error :: some_error :: Cmdliner.Cmd.Exit.defaults
  end
end

module Conf = struct
  type t =
    { log_level : Log.level;
      tty_cap : B0_std.Tty.cap;
      app_dir : Fpath.t }

  let v ~log_level ~tty_cap ~app_dir () = { log_level; tty_cap; app_dir  }
  let log_level c = c.log_level
  let tty_cap c = c.tty_cap
  let app_dir c = c.app_dir
  let users_file c = Fpath.(c.app_dir / "users.json")
  let authentication_private_key c = Fpath.(c.app_dir / "auth.private")
  let data_dir c = Fpath.(c.app_dir / "data")
  let static_dir c = Fpath.(c.app_dir / "static")
  let doi_cache_dir c = Fpath.(c.app_dir / "dois")
  let db_file c = Fpath.(data_dir c / "bib.sqlite3")
  let db_backup_file c = Fpath.(db_file c + ".backup")
  let bib_conf_file c = Fpath.(data_dir c / "bib-conf.json") (* FIXME unused *)

  let ensure_data_dir c =
    Result.map ignore (Os.Dir.create ~make_path:true (data_dir c))

  let with_cli log_level tty_cap app_dir =
    Result.map_error (fun e -> `Msg e) @@
    let log_level = B0_cli.B0_std.get_log_level log_level in
    let tty_cap = B0_cli.B0_std.get_tty_cap tty_cap in
    B0_cli.B0_std.setup tty_cap log_level ~log_spawns:Log.Debug;
    let* app_dir = match app_dir with
    | Some app_dir -> Ok app_dir
    | None ->
        let* dir = Os.Dir.cwd () in
        let db_file = db_file (v ~log_level ~tty_cap ~app_dir:dir ()) in
        let* exists = Os.Path.exists db_file in
        if exists then Ok dir else
        Fmt.error
          "Working directory is not an application directory.\n\
           %a: Use option %a to specify one or %a to use this directory\n\
          \      as an application directory."
                  Fmt.(tty' [`Fg `Yellow]) "Hint"
                  Fmt.code' "-a" Fmt.code' "-a ."
    in
    let* app_dir = Os.Path.realpath app_dir in
    Ok (v ~log_level ~tty_cap ~app_dir ())
end

module Cli = struct
  open Cmdliner

  let fpath = B0_cli.fpath

  let docs = Manpage.s_common_options
  let conf =
    let log_level =
      let env = Cmd.Env.info "HYPERBIB_VERBOSITY" in
      B0_cli.B0_std.log_level ~docs ~env ()
    in
    let tty_cap =
      let env = Cmd.Env.info "HYPERBIB_COLOR" in
      B0_cli.B0_std.tty_cap ~docs ~env ()
    in
    let app_dir =
      let doc = "Application directory. If unspecified defaults to the \
                 current working directory."
      and docv = "APP_DIR" in
      let env = Cmd.Env.info "HYPERBIB_APP_DIR" in
      Arg.(value & opt (some ~none:"." fpath) None &
           info ["a"; "app-dir"] ~doc ~docv ~env)
    in
    Term.term_result Term.(const Conf.with_cli $ log_level $ tty_cap $ app_dir)
end

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
