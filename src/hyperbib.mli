(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hyperbib standard needs.

    Adds a bit of functionality to the Stdlib and avoids
    too many leading opens. *)

module Std : sig

  (** {1:std Standard libray needs}

      For now we piggy back on B0_std but we should not. *)

  module Char = B0_std.Char
  module String = B0_std.String
  module Result = B0_std.Result
  module Fmt = B0_std.Fmt
  module List = B0_std.List
  module Fpath = B0_std.Fpath
  module Log = B0_std.Log
  module Cmd = B0_std.Cmd
  module Os = B0_std.Os

  (** Other needs *)
  module Bazaar : sig

    (* FIXME it these kinds of things should likely be added to
       B0_std *)

    val cp_dir_content :
      ?dotfiles:bool -> ?follow_symlinks:bool -> recurse:bool ->
      of_dir:Fpath.t -> inside_dir:Fpath.t -> unit -> (unit, string) result
  end

  (** {1:rel Rel open}

      {b FIXME try to get rid of most of these}.  *)

  module Bag = Rel_query.Bag

  (** {1:webs Webs_* opens} *)

  module Http = Webs.Http
  module Res = Webs_bazaar.Res
  module Kurl = Webs_bazaar.Kurl
  module At = Htmlit.At
  module El = Htmlit.El
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

  val v :
    log_level:Log.level -> tty_cap:B0_std.Tty.cap -> app_dir:Fpath.t ->
    unit -> t
  (** [v ~log_level ~tty_cap ~app_dir ()] is a configuration with given
      atributes. See the acessors for semantics. *)

  val log_level : t -> Log.level
  (** [log_level c] is the desired log level in [c] *)

  val tty_cap : t -> B0_std.Tty.cap
  (** [tty_cap c] is the terminal capability to assume for output. *)

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

  val fpath : Fpath.t Cmdliner.Arg.conv
  (** [fpath] is a filepath converter. *)

  (** {1:conf Cli configuration} *)

  val conf : Conf.t Cmdliner.Term.t
  (** [conf] is a cmdliner term for configuration.

      Term evaluation sets up logging level and color output
      by side effect. *)
end
