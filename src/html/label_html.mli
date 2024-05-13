(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Label HTML parts. *)

open Hyperbib_std


val form : edit:bool -> Kurl.fmt -> Label.t -> El.html
val label : Kurl.fmt -> Label.t -> El.html
val index : Page.Gen.t -> Label.t list -> Page.t
