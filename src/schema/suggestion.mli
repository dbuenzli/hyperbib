(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reference suggestions. *)

open Hyperbib_std


(** The type for suggestion ids. These are allocated by the database. *)
module Id : Rel_kit.INTABLE_ID

type t
(** The type for suggestions. *)

val make :
  id:Id.t -> timestamp:int -> doi:Doi.t option -> suggestion:string ->
  comment:string -> email:string -> unit -> t
(** [make …] is a suggestion with given properties. See accessors for
    semantics. *)

val row : Id.t -> int -> Doi.t option -> string -> string -> string -> t
(** [row …] is unlabelled {!make}. *)

val new' : t
(** [new] is a new suggestion. *)

val id : t -> Id.t
(** [id s] is the unique identifier of [s]. *)

val timestamp : t -> int
(** [timestamp s] is the time when the suggestion made it to the database. *)

val doi : t -> Doi.t option
(** [doi s] is the DOI of the suggestion if any. *)

val suggestion : t -> string
(** [suggestion s] is the free form text of the suggestion. *)

val comment : t -> string
(** [comment s], if any, is a private comment about the suggestion. *)

val email : t -> string
(** [email s], if any, is a private email to contact the suggester. *)

(** {1:table Table and queries} *)

val id' : (t, Id.t) Rel.Col.t
val timestamp' : (t, int) Rel.Col.t
val doi' : (t, Doi.t option) Rel.Col.t
val suggestion' : (t, string) Rel.Col.t
val comment' : (t, string) Rel.Col.t
val email' : (t, string) Rel.Col.t
val table : t Rel.Table.t

include Entity.IDENTIFIABLE_WITH_QUERIES with type t := t and module Id := Id
(** @inline *)

val list : (t, Bag.unordered) Rel_query.Bag.t
val list_stmt : t Rel_sql.Stmt.t

val find_doi : string Rel_query.value -> (t, Bag.unordered) Bag.t

(** {1:urls Urls} *)

module Url : sig
  type t =
  | Create
  | Confirm_delete of Id.t
  | Delete of Id.t
  | Fill_in
  | Index
  | Page of { id : Id.t; created : bool }
  | View_fields of Id.t

  val kind : t Kurl.kind
  val v : t -> Kurl.t
end
