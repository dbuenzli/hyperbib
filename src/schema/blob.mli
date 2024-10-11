(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** Blobs.

    Blobs are not stored in the databaes but in a {!Blobstore} which
    gives the identifiers of the rows of this table which holds metadata
    about blobs. *)

(** Blob identifiers (allocated by {!Blobstore}). *)
module Id : Entity.ID with type t = Blobstore.Key.text

type id = Id.t
(** The type for blob identifiers. *)

type t
(** the type for blobs. *)

val make :
  id:id -> media_type:Media_type.t -> origin:string -> public:bool ->
  slug:string -> t
(** [make] is a blob with given attributes. See accessors for semantics. *)

val row : id -> Media_type.t -> string -> bool -> string -> t
(** [row] is unlabelled {!make}. *)

val id : t -> id
(** [id d] is the unique identifier of [d] this is the XXH3-128 hash
    of the document in hexadecimal. *)

val media_type : t -> Media_type.t
(** [media_type d] is the media type of the document. *)

val public : t -> bool
(** [public d] indicates if the blob can be made public. *)

val origin : t -> string
(** [origin d] is tne origin of the document. For example the DOI
    resolver that was used for looking it up. *)

val slug : t -> string
(** [slug d] is a slug for the document. This is used for deriving a
    filename. If empty {!id} can be used or a scheme derived from
    the entity to which it is attached. *)

(** {1:table Table} *)

open Rel

val id' : (t, id) Col.t
(** [id'] is the {!id} column. *)

val media_type' : (t, Media_type.t) Col.t
(** [media_type'] is the {!media_type} column. *)

val public' : (t, bool) Col.t
(** [public'] is the {!public} column. *)

val origin' : (t, string) Col.t
(** [origin'] is the {!origin} column. *)

val slug' : (t, string) Col.t
(** [slug] is the {!slug} column. *)

val table : t Table.t
(** [table] is the document table. *)

(** {1:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id
