(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Table identifiers.

    Should we generate new abstract types for each table ?

    Could avoid some type errors.
    Could be painful.
    Could introduce more circular deps.
    But try it.
*)

type t = int
(** The type for table identifiers. *)

module Set : Set.S with type elt = t
module Map : sig
  include Map.S with type key = t

  val of_list : ('a -> key) -> 'a list -> 'a t

  (** {1:add Additional adds and gets} *)

  val add_to_list : key -> 'a -> 'a list t -> 'a list t
  (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
        [v :: find k m] if [k] was bound in [m] and [[v]] otherwise. *)

  val add_to_set :
    (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
    key -> 'a -> 'set t -> 'set t
  (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
      [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
      otherwise. *)

  val get_list : key -> 'a list t -> 'a list
  (** [get_list k m] is the list bound to [k] in [m] or the empty
      list if [k] is unbound. *)
end

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
