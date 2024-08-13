(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hyperbib app configuration and cli interaction. *)

open Hyperbib_std

(** General [hyperbib] tool configuration. *)
module Conf : sig

  type t
  (** The type for configurations. *)

  val make :
    log_level:Log.level -> fmt_styler:Fmt.styler -> app_dir:Fpath.t ->
    http_client:(Http_client.t, string) result -> unit -> t
  (** [make] is a configuration with given atributes. See the acessors
      for semantics. *)

  val log_level : t -> Log.level
  (** [log_level c] is the desired log level in [c] *)

  val fmt_styler : t -> Fmt.styler
  (** [fmt_styler c] is the formatter styler. *)

  val app_dir : t -> Fpath.t
  (** [app_dir c] is the absolute path to the application directory. *)

  val users_file : t -> Fpath.t
  (** [users_file c] is the JSON file holding application users. *)

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

  (*
  val bib_conf_file : t -> Fpath.t
  (** [bib_conf_file c] is the file holding the bibliography configuration. *)
*)

  val authentication_private_key : t -> Fpath.t
  (** [authentication_private_key c] is the file that stores the private key
      to authenticate data like session cookies. *)

  val http_client : t -> (Http_client.t, string) result
  (** [http_client c] is the HTTP client to use in the app. *)
end

val with_db_transaction :
  Conf.t -> Db.transaction_kind ->
  (Db.t -> ('a, string) result) -> ('a, string) result



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

open Cmdliner

val fpath : Fpath.t Arg.conv
(** [fpath] is a filepath converter. *)

(** {1:conf Cli configuration} *)

val conf : Conf.t Term.t
(** [conf] is a cmdliner term for configuration.

      Term evaluation sets up logging level and color output by side
      effect and looksup the HTTP client. *)

val cmd_with_conf :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  string -> (Conf.t -> 'a) Term.t -> 'a Cmd.t

val cmd :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  string -> 'a Term.t -> 'a Cmd.t

val cmd_group :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  ?default:'a Cmdliner.Term.t -> string -> 'a Cmd.t list -> 'a Cmdliner.Cmd.t
