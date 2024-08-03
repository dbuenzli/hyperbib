(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Static HTML generation. *)

open Hyperbib.Std

val cmd : Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [html]. *)
