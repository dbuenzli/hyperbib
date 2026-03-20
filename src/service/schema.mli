(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Database schema. *)

open Hyperbib_std

type config = string * string
val config : config Rel.Table.t

val version : int
val v : Rel.Schema.t
