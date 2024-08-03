(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Database schema. *)

open Hyperbib.Std

type conf = string * string
val conf : conf Rel.Table.t

val version : int
val v : Rel.Schema.t
