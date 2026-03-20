(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Cmdliner

(** Exit codes. *)
module Exit : sig

  val ok : Os.Exit.t
  (** [ok] is the zero exit code. *)

  val user_exists : Os.Exit.t
  (** [user_exists] indicates a user already exists. *)

  val some_error : Os.Exit.t
  (** [some_error] indicates an indiscriminate error reported on stderr. *)

  module Info : sig
    val user_cmd : Cmdliner.Cmd.Exit.info list
  end
end

(** {1:cli Cli fragments} *)

val config : Hyperbib_config.t Term.t
(** [config] is a cmdliner term for configuration. Term evaluation sets up
    logging level and coloring by side effect and looks up the HTTP client. *)
