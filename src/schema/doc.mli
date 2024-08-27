(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** Documents.

    Documents can be associated to references. Generally they are the
    content of the reference. *)

type xxh3_128_hex = string
(** The type for hexadecimal XXH3-128 hashes. *)

type id = xxh3_128_hex
(** The type for document identifiers. *)

type t
(** the type for documents. *)

val v :
  id:id -> reference:Reference.id option -> slug:string ->
  media_type:Media_type.t -> public:bool -> origin:string -> t
(** [v] is document with given attributes. See accessors for semantics. *)

val row :
  id -> Reference.id option -> string -> Media_type.t -> bool -> string -> t
(** [row] is unlabelled {!v}. *)

val id : t -> id
(** [id d] is the unique identifier of [d] this is the XXH3-128 hash
    of the document in hexadecimal. *)

val reference : t -> Reference.id option
(** [reference d] is the reference to which [d] is attached. If any.
    If this is [None] the reference was deleted. *)

val slug : t -> string
(** [slug d] is a slug for the document. This is used for
    deriving a filename. If empty one is derived from the reference
    if any or the {!id} as a last resort. *)

val media_type : t -> Media_type.t
(** [media_type d] is the media type of the document. *)

val public : t -> bool
(** [public d] indicates if the document can be made public. *)

val origin : t -> string
(** [origin d] is tne origin of the document. For example the DOI
    resolver that was used for looking it up. *)

(** {1:table Table} *)

open Rel

val id' : (t, id) Col.t
(** [id'] is the {!id} column. *)

val reference' : (t, Reference.id option) Col.t
(** [reference'] is the {!reference} column. *)

val slug' : (t, string) Col.t
(** [slug] is the {!slug} column. *)

val media_type' : (t, Media_type.t) Col.t
(** [media_type'] is the {!media_type} column. *)

val public' : (t, bool) Col.t
(** [public'] is the {!public} column. *)

val origin' : (t, string) Col.t
(** [origin'] is the {!origin} column. *)

val table : t Table.t
(** [table] is the document table. *)
