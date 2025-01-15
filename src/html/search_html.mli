(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Search renderings. *)

open Hyperbib_std

val index :
  ?results:El.html -> Page.Gen.t -> Search.raw_query option -> Page.t
