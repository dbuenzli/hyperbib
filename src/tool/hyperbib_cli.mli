(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

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

val conf : Hyperbib_conf.t Term.t
(** [conf] is a cmdliner term for configuration.

      Term evaluation sets up logging level and color output by side
      effect and looksup the HTTP client. *)

val cmd_with_conf :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  string -> (Hyperbib_conf.t -> 'a) Term.t -> 'a Cmd.t

val cmd :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  string -> 'a Term.t -> 'a Cmd.t

val cmd_group :
  ?doc:string -> ?man:Manpage.block list -> ?exits:Cmd.Exit.info list ->
  ?default:'a Cmdliner.Term.t -> string -> 'a Cmd.t list -> 'a Cmdliner.Cmd.t
