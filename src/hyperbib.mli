(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Hyperbib standard needs.

    Adds a bit of functionality to the Stdlib and avoids
    too many leading opens. *)

module Std : sig

  (** {1:std Standard libray needs}

      For now we piggy back on B00_std but we should not. *)

  module Char = B00_std.Char
  module String = B00_std.String
  module Result = B00_std.Result
  module Fmt = B00_std.Fmt
  module List = B00_std.List
  module Fpath = B00_std.Fpath
  module Log = B00_std.Log
  module Os = B00_std.Os

  (** Other needs *)
  module Bazaar : sig

    (* FIXME it these kinds of things should likely be added to
       B00_std *)

    val list_fold_stop_on_error :
      ('a -> 'b -> ('b, 'err) result) -> 'a list -> 'b -> ('b, 'err) result

    val list_iter_stop_on_error :
      ('a -> ('b, 'err) result) -> 'a list -> (unit, 'err) result

    val list_iter_log_on_error :
      ('a -> ('b, string) result) -> 'a list -> unit

    val cp_dir_content :
      ?dotfiles:bool -> ?follow_symlinks:bool -> recurse:bool ->
      of_dir:Fpath.t -> inside_dir:Fpath.t -> unit -> (unit, string) result
  end


  (** {1:ask Ask open}

      This is open {!Ask.Std}. *)

  module Type = Ask.Type
  module Col = Ask.Col
  module Row = Ask.Row
  module Index = Ask.Index
  module Table = Ask.Table
  module Bag = Ask.Bag
  module Sql = Ask.Sql

  (** {1:webs Webs_* opens} *)

  module Http = Webs.Http
  module Session = Webs_kit.Session
  module Res = Webs_kit.Res
  module Kurl = Webs_kit.Kurl
  module At = Webs_html.At
  module El = Webs_html.El
end

open Std

(** Exit codes. *)
module Exit : sig

  val ok : Os.Exit.t
  (** [ok] is the zero exit code. *)

  val user_exists : Os.Exit.t
  (** [user_exists] indicates a user already exists. *)

  val conf_error : Os.Exit.t
  (** [conf_error] indicates a configuration error. *)

  val some_error : Os.Exit.t
  (** [some_error] indicates an indiscriminate error reported on stderr. *)

  (** Cmdliner documentation. *)
  module Info : sig
    val base_cmd : Cmdliner.Cmd.Exit.info list
  end
end

(** General [hyperbib] tool configuration. *)
module Conf : sig

  type t
  (** The type for configurations. *)

  val v : log_level:Log.level -> tty_cap:B00_std.Tty.cap -> unit -> t
  (** [v ~app_dir ~log_level ~tty_cap ()] is a coniguration with given
      atributes. See the acessors for semantics. *)

  val log_level : t -> Log.level
  (** [log_level c] is the desired log level in [c] *)

  val tty_cap : t -> B00_std.Tty.cap
  (** [tty_cap c] is the terminal capability to assume for output. *)
end

(** Application data specific configuration. *)
module Data_conf : sig

  type t
  (** The type for data configurations. *)

  val v : app_dir:Fpath.t -> unit -> t
  (** [v ~app_dir ()] is a configuration with given
      atributes. See the acessors for semantics. *)

  val app_dir : t -> Fpath.t
  (** [app_dir c] is the absolute path to the application directory. *)

  val users_file : t -> Fpath.t
  (** [users_file c] is the JSON file holding application users. *)

  val data_dir : t -> Fpath.t
  (** [data_dir c] is the absolute path to the data directory. *)

  val doi_cache_dir : t -> Fpath.t
  (** [doic_cache_dir c] is the absolute path to the DOI metadata
      cache directory. *)

  val static_dir : t -> Fpath.t
  (** [static_dir c] is the absolute path to the static assets directory. *)

  val db_file : t -> Fpath.t
  (** [db_file c] is the sqlite3 database file holding application data. *)

  val db_backup_file : t -> Fpath.t
  (** [db_backup_file c] is the stable backup of the sqlite3 database file
      holding application data. *)

  val bib_conf_file : t -> Fpath.t
  (** [bib_conf_file c] is the file holding the bibliography configuration. *)

  val authentication_private_key : t -> Fpath.t
  (** [authentication_private_key c] is the file that stores the private key
      to authenticate data like session cookies. *)

  (** {1:ops Operations} *)

  val ensure_data_dir : t -> (unit, string) result
  (** [ensure_data_dir c] makes sure {!data_dir} exists. *)
end

(** Cli interaction. *)
module Cli : sig

  (** {1:conf Cli configuration} *)

  val conf : Conf.t Cmdliner.Term.t
  (** [conf] is a cmdliner term for configuration.

      Term evaluation sets up logging level and color output
      by side effect. *)

  val data_conf : pos:int -> Data_conf.t Cmdliner.Term.t
  (** [app_conf ~pos] is a cmdliner term for application
      configuration. [pos] indicates the mandatory positional argument
      for {!Data_conf.app_dir} argument. *)
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
