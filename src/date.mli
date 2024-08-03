(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

type day = int
type month = int
type year = int

(** {1:md_partial month day partial} *)

type md_partial = month * day option
val md_partial_of_string : string -> (md_partial, string) result
val pp_md_partial : Format.formatter -> md_partial -> unit

val ask_md_partial_type : md_partial Rel.Type.t

(** {1:partial Partial date} *)

type partial = year * md_partial option
(** A partial date is a date of the form YYYY[-MM[-DD]]. *)

val partial_of_string : string -> (partial, string) result
(** [partial_of_string s] parses a partial date from [s]. *)

val partial_to_string : partial -> string

val pp_partial : Format.formatter -> partial -> unit
(** [pp] is a formatter for partial dates. *)

val ask_partial_type : partial Rel.Type.t
(** N.B. no longer used. *)
