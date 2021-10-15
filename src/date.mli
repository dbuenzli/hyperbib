(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

type day = int
type month = int
type year = int

(** {1:md_partial month day partial} *)

type md_partial = month * day option
val md_partial_of_string : string -> (md_partial, string) result
val pp_md_partial : Format.formatter -> md_partial -> unit

val ask_md_partial_type : md_partial Type.t

(** {1:partial Partial date} *)

type partial = year * md_partial option
(** A partial date is a date of the form YYYY[-MM[-DD]]. *)

val partial_of_string : string -> (partial, string) result
(** [partial_of_string s] parses a partial date from [s]. *)

val partial_to_string : partial -> string

val pp_partial : Format.formatter -> partial -> unit
(** [pp] is a formatter for partial dates. *)

val ask_partial_type : partial Type.t
(** N.B. no longer used. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
