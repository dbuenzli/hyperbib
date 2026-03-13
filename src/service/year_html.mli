(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Year renderings. *)

open Hyperbib_std

val page :
  Page.Gen.t -> year:Date.year option -> Reference.render_data -> Page.t

val index : Page.Gen.t -> (Date.year option * int) list -> Page.t
