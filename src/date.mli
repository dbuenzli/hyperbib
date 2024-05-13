(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type day = int
type month = int
type year = int

(** {1:md_partial month day partial} *)

type md_partial = month * day option
val md_partial_of_string : string -> (md_partial, string) result
val md_partial_to_string : md_partial -> string
val pp_md_partial : Format.formatter -> md_partial -> unit

(** {1:partial Partial date} *)

type partial = year * md_partial option
(** A partial date is a date of the form YYYY[-MM[-DD]]. *)

val partial_of_string : string -> (partial, string) result
(** [partial_of_string s] parses a partial date from [s]. *)

val partial_to_string : partial -> string

val pp_partial : Format.formatter -> partial -> unit
(** [pp] is a formatter for partial dates. *)
