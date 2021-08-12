(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** References by year pages. *)

open Hyperbib.Std

val filter :
  year:Date.year Ask.value -> (Reference.t, 'a) Bag.t ->
  (Reference.t, Bag.unordered) Bag.t
(** [filter ~year refs] are the reference of [refs] which have [year]. *)

val public_domain_stmt : (Date.year * int) Sql.Stmt.t


module Url : sig
  type t =
  | Index
  | Page of int

  val kind : t Kurl.kind
  val v : t -> Kurl.t
  val page : int -> Kurl.t
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern.

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
