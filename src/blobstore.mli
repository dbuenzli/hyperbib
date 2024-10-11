(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** Blob store.

    Binary objects are keyed by the XXH3-128 hash of their
    content however this doesn't surface at the API level and the scheme
    allows to add other hashes in case we start running into
    collisions.

    {!Key.to_text} is the reference to store in the database. *)

(** Document keys. *)
module Key : sig

  type t
  (** The type for blob keys. *)

  type text = string
  (** The type for key in US-ASCII hexadecimal. *)

  val to_text : t -> text
  (** [to_text k] is [k] as text. This can be used for serializing keys. *)

  val of_text : text -> (t, string) result
  (** [of_text t] is a key from text [t]. *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k0] and [k1] are the same key. *)

  val compare : t -> t -> int
  (** [compare] is a total order on keys compatible with {!equal}. *)

  val pp : t Fmt.t
  (** [pp] is a formatter for keys. *)
end

type t
(** The type for blob stores. *)

val of_dir : Fpath.t -> (t, string) result
(** [of_dir dir] is a blob store reading and writing in [dir].
    If [dir] does not exist it is created when needed. *)

val dir : t -> Fpath.t
(** [dir store] is the directory of the blob store (which may not exist
    yet). *)

type add_status =
| Created (** The data is new. *)
| Exists (** The data is already in the store. *)
| Collides (** There is a hash collision. *)
(** The type for additions results. *)

val add : Bytes.Reader.t -> t -> (Key.t * add_status, string) result
(** [add blob store] adds [blob] to [store] and returns its key and
    the addition status. If [Collision] is returned the [store] is
    left untouched. *)

val mem : Key.t -> t -> (bool, string) result
(** [mem key store] is [Ok true] iff there is a blob keyed by [key]
    in [store]. *)

val delete : Key.t -> t -> (bool, string) result
(** [delete key store] deletes the blob keyed by [key] in [store] and
    reports if the blob existed or not. *)

val find : Key.t -> t -> (Fpath.t option, string) result
(** [find key store] is the path in [store] to the blob keyed by [key]
    (if any). *)

val with_blob : Key.t -> t ->
  (Bytes.Reader.t option -> 'a) -> ('a, string) result
(** [with_blob key store f] makes blob keyed by [key] in [store]
    available in [f] (if any). The reader is only valid during [f]. *)

val fold :
  (Key.t option -> Fpath.t -> 'a -> 'a) -> t -> 'a -> ('a, string) result
(** [fold f store acc] folds [f] over the blobs found in [store]
    starting with [acc]. If [f] is called with [None] as the first
    argument then the path is in the directory [dir store] but
    not keyed by the stored. *)
