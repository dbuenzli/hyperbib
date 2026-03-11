(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** Search. *)

type raw_query = string

module Url : sig
  val query_key : string
  type t =
  | Index of raw_query option
  | Results of raw_query option

  val kind : t Kurl.kind
  val v : t -> Kurl.t
end
