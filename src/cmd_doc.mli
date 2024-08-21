(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Run maintenance tasks.  *)

open Hyperbib_std

val cmd : Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [run]. *)
