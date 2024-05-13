(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Static files mentioned in HTML. *)

open Hyperbib_std

module Url : sig
  type t =
  | Hyperbib_css
  | Hyperbib_js

  val kind : t Kurl.kind
  val v : t -> Kurl.t
end
