(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Bytesrw_xxhash

(** Document store.

    Documents are keyed by the XXH3-128 hash of their content. *)

type key = Xxh3_128.t
(** The type for document keys. *)

type t
(** The type for document stores. *)

val of_dir : Fpath.t -> (t, string) result
(** [of_dir dir] is a document store reading from directory [dir]. *)

val dir : t -> Fpath.t
(** [dir store] is the directory of the document store. *)

val add : Bytes.Reader.t -> t -> (key, string) result
(** [add doc store] adds to [store] the document [doc] and returns its
    key. Errors if they is a key collision. *)

val mem : key -> t -> (bool, string) result
(** [mem key store] is [Ok true] iff there is a document keyed by [key]
    in [store]. *)

val delete : key -> t -> (bool, string) result
(** [delete key store] delete the document keyed by [key] in [store] and
    reports if the document existed or not. *)

val find : key -> t -> (Fpath.t option, string) result
(** [find key store] is the path in [store] to the document keyed by [key]
    (if any). *)

val with_doc : key -> t -> (Bytes.Reader.t option -> 'a) -> ('a, string) result
(** [with_doc key store f] makes document keyed by [key] in [store]
    available in [f] (if any). The reader is only valid during [f]. *)

val fold :
  (key option -> Fpath.t -> 'a -> 'a) -> t -> 'a -> ('a, string) result
(** [fold f store acc] folds [f] over the documents found in [store]
    starting with [acc]. If [f] is called with [None] as the first
    argument then the path is not keyed by an XXH3-128 filename. *)
