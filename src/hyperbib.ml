(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Std = struct

  (* Standard needs. *)

  module Char = B00_std.Char
  module String = B00_std.String
  module Result = B00_std.Result
  module Fmt = B00_std.Fmt
  module Log = B00_std.Log
  module List = B00_std.List
  module Fpath = B00_std.Fpath
  module Os = B00_std.Os

  module Bazaar = struct

    let rec list_fold_stop_on_error f l acc = match l with
    | [] -> Ok acc
    | v :: vs ->
        match f v acc with
        | Ok acc -> list_fold_stop_on_error f vs acc
        | Error _ as e -> e

    let rec list_iter_stop_on_error f = function
    | [] -> Ok ()
    | v :: vs ->
        match f v with Error _ as e -> e | Ok v -> list_iter_stop_on_error f vs

    let rec list_iter_log_on_error f = function
    | [] -> ()
    | v :: vs ->
        (match f v with Error _ as e -> Log.if_error ~use:() e | Ok _ -> ());
        list_iter_log_on_error f vs

    let cp_dir_content
        ?dotfiles ?follow_symlinks ~recurse ~of_dir:src ~inside_dir:dst ()
      =
      let cp _ _ rel () =
        Log.if_error ~use:() @@
        let src = Fpath.(src // rel) and dst = Fpath.(dst // rel) in
        Os.File.copy ~force:true ~make_path:true ~src dst
      in
      Os.Dir.fold_files ?dotfiles ?follow_symlinks ~rel:true ~recurse cp src ()
  end

  (* Ask needs. *)

  module Type = Ask.Type
  module Col = Ask.Col
  module Row = Ask.Row
  module Index = Ask.Index
  module Table = Ask.Table
  module Bag = Ask.Bag
  module Sql = Ask.Sql

  (* Webs needs *)

  module Http = Webs.Http
  module Session = Webs_kit.Session
  module Res = Webs_kit.Res
  module Kurl = Webs_kit.Kurl

  module At = Webs_html.At
  module El = Webs_html.El

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
  type t = { log_level : Log.level; tty_cap : B00_std.Tty.cap; }

  let v ~log_level ~tty_cap () = { log_level; tty_cap; }
  let log_level c = c.log_level
  let tty_cap c = c.tty_cap

  let with_cli log_level tty_cap =
    Result.map_error (fun e -> `Msg e) @@
    let log_level = B00_cli.B00_std.get_log_level log_level in
    let tty_cap = B00_cli.B00_std.get_tty_cap tty_cap in
    B00_cli.B00_std.setup tty_cap log_level ~log_spawns:Log.Debug;
    Ok (v ~log_level ~tty_cap ())
end

module Data_conf = struct
  type t = { app_dir : Fpath.t; }

  let v ~app_dir () = { app_dir; }
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

  let with_cli app_dir =
    Result.map_error (fun e -> `Msg e) @@
    let* app_dir = Os.Path.realpath app_dir in
    Ok (v ~app_dir ())
end

module Cli = struct
  open Cmdliner

  let docs = Manpage.s_common_options
  let conf =
    let log_level =
      let env = Cmd.Env.info "HYPERBIB_VERBOSITY" in
      B00_cli.B00_std.log_level ~docs ~env ()
    in
    let tty_cap =
      let env = Cmd.Env.info "HYPERBIB_COLOR" in
      B00_cli.B00_std.tty_cap ~docs ~env ()
    in
    Term.term_result Term.(const Conf.with_cli $ log_level $ tty_cap)

  let data_conf ~pos:p =
    let app_dir =
      let doc = "Application directory." and docv = "APP_DIR" in
      Arg.(required & pos p (some B00_cli.fpath) None & info [] ~doc ~docv)
    in
    Term.term_result Term.(const Data_conf.with_cli $ app_dir)
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
