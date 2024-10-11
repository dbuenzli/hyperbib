(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
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


module Rel : sig
  val type' : t Rel.Type.t
  val v : t -> t Rel_query.value
  val equal : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  val ( = ) : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
end
