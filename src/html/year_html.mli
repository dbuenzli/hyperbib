(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Year renderings. *)

open Hyperbib.Std

val page : Page.Gen.t -> year:int -> Reference.render_data -> Page.t
val index : Page.Gen.t -> (Date.year * int) list -> Page.t
