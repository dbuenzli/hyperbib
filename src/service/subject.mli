(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Subjects.

    Classifies references. *)

open Hyperbib.Std

(** {1:subjects Subjects} *)

type id = Id.t
(** The type for subject ids. These are allocated by the database. *)

type t
(** The type for subjects. *)

type subject = t
(** See {!t}. *)

val v :
  id:id -> name:string -> parent:id option -> see:id option ->
  description:string -> private_note:string -> public:bool -> unit ->  t
(** [v …] is a subject with given attributes, see accessors for semantics. *)

val row :
  id -> string -> id option -> id option -> string -> string -> bool -> t
(** [row] is unlabelled {!v}. *)

val new' : t
(** [new'] is a new subject. *)

val id : t -> id
(** [id s] is the unique identifier of [s].  *)

val name : t -> string
(** [name s] is the name of [s]. *)

val parent : t -> id option
(** [parent s] is the identifier of the parent subject of [s] (if any). *)

val see : t -> id option
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
    none. The {!id} is the one of [s]. *)

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

val id' : (t, id) Col.t
(** [id s] is the unique identifier of [s].  *)

val name' : (t, string) Col.t
(** [name'] is the column for {!name}. *)

val parent' : (t, id option) Col.t
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

  val v : given:id -> that:id -> unit -> t
  (** [v given see] indicates interest in [given] subject should
      also consult [that]. *)

  val row : id -> id -> t
  (** [row] is unlabelled {!v}. *)

  val given : t -> id
  (** [given s] is the source subject. *)

  val that : t -> id
  (** [that s] is the subject to consult. *)

  (** {1:table Table} *)

  val given' : (t, id) Col.t
  (** [given'] is the column for {!val-given}. *)

  val that' : (t, id) Col.t
  (** [that'] is the column for {!val-that}. *)

  val table : t Table.t
  (** The table for "see also" subject relationships. *)

  (** {1:queries Queries} *)

  val create : t -> unit Rel_sql.Stmt.t
  (** [create sa] creates a see also relationship. *)
end

(** Subject label applications. *)
module Label : Label.APPLICATION
  with type entity := t and type entity_id := id

(** {1:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id

val visible : t Rel_query.value -> bool Rel_query.value
val visible_list : (t, Bag.unordered) Bag.t
val list_visibility : (t * bool, Bag.unordered) Bag.t
val list_visibility_stmt : (t * bool) Rel_sql.Stmt.t

val parents : (t, Bag.unordered) Bag.t
val parents_stmt : t Rel_sql.Stmt.t

val children : id Rel_query.value -> (t, Bag.unordered) Bag.t
val children_stmt : id -> t Rel_sql.Stmt.t

val select : string Rel_query.value -> (t, Bag.unordered) Bag.t
val select_stmt : string -> t Rel_sql.Stmt.t

(** {1:url Urls} *)

(** Subject URL requests *)
module Url : sig

  (** {1:url_req URL requests} *)

  type named_id = string option * id

  type t =
  | Confirm_delete of id
  | Create
  | Delete of id
  | Duplicate of id
  | Duplicate_form of id
  | Edit_form of id
  | Index
  | Input of Entity.Url.for_list * Entity.Url.input_name * id
  | Input_create of Entity.Url.for_list * Entity.Url.input_name * subject
  | Input_finder of Entity.Url.for_list * Entity.Url.input_name
  | Input_finder_find of Entity.Url.for_list * Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Update of id
  | View_fields of id (** *)
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

(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern

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
