(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Subjects.

    Classifies references. *)

open Hyperbib_std

(** {1:subjects Subjects} *)

(** The type for subject ids. These are allocated by the database. *)
module Id : Rel_kit.INT_ID

type t
(** The type for subjects. *)

type subject = t
(** See {!t}. *)

val make :
  id:Id.t -> name:string -> parent:Id.t option -> see:Id.t option ->
  description:string -> private_note:string -> public:bool -> unit ->  t
(** [make …] is a subject with given attributes, see accessors for semantics. *)

val row :
  Id.t -> string -> Id.t option -> Id.t option -> string -> string -> bool -> t
(** [row] is unlabelled {!make}. *)

val new' : t
(** [new'] is a new subject. *)

val id : t -> Id.t
(** [id s] is the unique identifier of [s].  *)

val name : t -> string
(** [name s] is the name of [s]. *)

val parent : t -> Id.t option
(** [parent s] is the identifier of the parent subject of [s] (if any). *)

val see : t -> Id.t option
(** [see s] is the identifier of the subject that is used instead of [s]. *)

val description : t -> string
(** [description s] is the description of [s]. *)

val private_note : t -> string
(** [private_note s] is the private note of [s]. *)

val public : t -> bool
(** [public s] is [true] iff [s]'s existence is made visible to the public. *)

(** {1:ops Operations} *)

val duplicate_data : t -> t
(** [duplicate_data s] is [s] with its name altered and {!public} set to
    none. The {!val-id} is the one of [s]. *)

(** {1:predcomp Predicates and comparisons} *)

val is_root : t -> bool
(** [is_root s] is [Option.is_none (parent s)]. *)

val order_by_name : t -> t -> int
(** [order_by_name s0 s1] orders [s0] and [s1] in lexicographic
    order of their name (we should likely let the DB do that). *)

(** {1:derived Derived data} *)

val hierarchy : t list -> t list * t list Id.Map.t
(** [hierarchy ss] is the list of subject roots and a map from roots to their
    children. Does not check that each element in [ss] has its root
    in [ss]. *)

(** {1:tables Tables} *)

open Rel

val id' : (t, Id.t) Col.t
(** [id s] is the unique identifier of [s].  *)

val name' : (t, string) Col.t
(** [name'] is the column for {!name}. *)

val parent' : (t, Id.t option) Col.t
(** [parent'] is the column for {!parent}. *)

val description' : (t, string) Col.t
(** [description'] is the column for {!description}. *)

val private_note' : (t, string) Col.t
(** [private_note'] is the column for {!private_note}. *)

val public' : (t, bool) Col.t
(** [public'] is the column for {!public}. *)

val table : t Table.t
(** [table] is the subject table. *)

(** See also relation. *)
module See_also : sig
  type t
  (** The type for the see also subject relation. *)

  val make : given:Id.t -> that:Id.t -> unit -> t
  (** [make given see] indicates interest in [given] subject should
      also consult [that]. *)

  val row : Id.t -> Id.t -> t
  (** [row] is unlabelled {!make}. *)

  val given : t -> Id.t
  (** [given s] is the source subject. *)

  val that : t -> Id.t
  (** [that s] is the subject to consult. *)

  (** {1:table Table} *)

  val given' : (t, Id.t) Col.t
  (** [given'] is the column for {!val-given}. *)

  val that' : (t, Id.t) Col.t
  (** [that'] is the column for {!val-that}. *)

  val table : t Table.t
  (** The table for "see also" subject relationships. *)

  (** {1:queries Queries} *)

  val create : t -> unit Rel_sql.Stmt.t
  (** [create sa] creates a see also relationship. *)
end

(** Subject label applications. *)
module Label : Label.APPLICATION
  with type entity := t and type entity_id := Id.t

(** {1:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and module Id := Id

val visible : t Rel_query.value -> bool Rel_query.value
val visible_list : (t, Bag.unordered) Bag.t
val list_visibility : (t * bool, Bag.unordered) Bag.t
val list_visibility_stmt : (t * bool) Rel_sql.Stmt.t

val parents : (t, Bag.unordered) Bag.t
val parents_stmt : t Rel_sql.Stmt.t

val children : Id.t Rel_query.value -> (t, Bag.unordered) Bag.t
val children_stmt : Id.t -> t Rel_sql.Stmt.t

val select : string Rel_query.value -> (t, Bag.unordered) Bag.t
val select_stmt : string -> t Rel_sql.Stmt.t

val id_map :
  Db.t -> 'a Rel_sql.Stmt.t -> ('a -> Id.t) -> ('a Id.Map.t, Db.error) result


(** {1:url Urls} *)

(** Subject URL requests *)
module Url : sig

  (** {1:url_req URL requests} *)

  type named_id = string option * Id.t

  type t =
  | Confirm_delete of Id.t
  | Create
  | Delete of Id.t
  | Duplicate of Id.t
  | Duplicate_form of Id.t
  | Edit_form of Id.t
  | Index
  | Input of Entity.Url.for_list * Entity.Url.input_name * Id.t
  | Input_create of Entity.Url.for_list * Entity.Url.input_name * subject
  | Input_finder of Entity.Url.for_list * Entity.Url.input_name
  | Input_finder_find of Entity.Url.for_list * Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of Id.t
  | Replace_form of Id.t
  | Update of Id.t
  | View_fields of Id.t (** *)
  (** The type for subject URL requests. *)

  val kind : t Kurl.kind
  (** The type for subject URL request kinds. *)

  val v : t -> Kurl.t
  (** [v u] is [Kurl.v kind u]. *)

  (** {1:cons Convenience constructors} *)

  val res_name : subject -> string
  (** [res_name s] an URL path segment for naming [s]. *)

  val page : subject -> Kurl.t
  (** [page s] is a {!Url.type-t.Page} URL request for [s]. *)
end
