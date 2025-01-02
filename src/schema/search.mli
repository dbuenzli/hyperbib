(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Search. *)

open Hyperbib_std

module Url : sig
  type t = Index
  val kind : t Kurl.kind
  val v : t -> Kurl.t
end
