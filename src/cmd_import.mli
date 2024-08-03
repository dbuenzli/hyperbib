(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Importing data. *)

open Hyperbib.Std

val cmd : Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [import]. *)
