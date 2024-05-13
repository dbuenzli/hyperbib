(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Containers.

    Containers of references. N.B. in a more serious system like an
    ILS that would be a reference itself. *)

open Hyperbib_std

(** {1 Containers} *)

type id = Id.t
(** The type for container ids. These are allocated by the database. *)

type t
(** The type for containers. *)

type container = t
(** See {!t}. *)

val v :
  id:id -> title:string -> isbn:string -> issn:string ->
  note:string -> private_note:string -> public:bool ->
  unit -> t
(** [v …] is a container with given attributes, see accessors for semantics. *)

val row : id -> string -> string -> string -> string -> string -> bool -> t
(** [row] is {!v} unlabelled. *)

val new' : t
(** [new'] is a new container. *)

val id : t -> id
(** [id c] is the unique identifier of [c]. *)

val title : t -> string
(** [title c] is the title of [c]. *)

val isbn : t -> string
(** [isbn c] is the ISBN of [c]. *)

val issn : t -> string
(** [issn c] is the ISSN of [c]. *)

val note : t -> string
(** [note c] is the note of [c]. *)

val private_note : t -> string
(** [private_note c] is the private note of [c]. *)

val public : t -> bool
(** [public c] is [true] iff [c]'s existence is made visible to the
    public *)

(** {1:ops Operations} *)

val duplicate_data : t -> t
(** [duplicate_data c] is [c] with its title altered and {!public} set
    to none. The {!val-id} is the one of [c]. *)

(** {1:predcomp Predicates and comparisons} *)

val order_by_title : t -> t -> int
(** [order_by_title c0 c1] orders [c0] and [c1] in lexicographic order
    by title. *)

(** {1:derived Derived data} *)

val index_letter : t -> char option
(** [index_letter c] is the letter under which [c] can be found
    in the index. {b FIXME} this should be made Unicode aware. *)

(** {1:tables Tables} *)

open Rel

val id' : (t, id) Col.t
(** [id'] is the column for {!val-id}. *)

val title' : (t, string) Col.t
(** [title'] is the column for {!val-title}. *)

val isbn' : (t, string) Col.t
(** [isbn'] is the column for {!val-isbn}. *)

val issn' : (t, string) Col.t
(** [issn'] is the column for {!val-issn}. *)

val note' : (t, string) Col.t
(** [note'] is the column for {!val-note}. *)

val private_note' : (t, string) Col.t
(** [private_note'] is the column for {!val-private_note'}. *)

val public' : (t, bool) Col.t
(** [public'] is the column for {!val-public'}. *)

val table : t Table.t
(** [table] is the container table. *)

(** Container label applications. *)
module Label : Label.APPLICATION
  with type entity := t and type entity_id := id

(** {1:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id

val match' :
  title:string Rel_query.value ->
  isbn:string Rel_query.value -> issn:string Rel_query.value ->
  (t, Bag.unordered) Rel_query.Bag.t

val match_stmt :
  title:string -> isbn:string -> issn:string -> t Rel_sql.Stmt.t

val select : string Rel_query.value -> (t, Bag.unordered) Rel_query.Bag.t
val select_stmt : string -> t Rel_sql.Stmt.t

(** {1:urls URLs} *)

(** Container URL requests. *)
module Url : sig

  (** {1:url_req URL request} *)

  type named_id = string option * id

  type t =
  | Confirm_delete of id
  | Create
  | Delete of id
  | Duplicate of id
  | Duplicate_form of id
  | Edit_form of id
  | Index
  | Input of Entity.Url.input_name * id
  | Input_create of Entity.Url.input_name * container
  | Input_finder of Entity.Url.input_name
  | Input_finder_find of Entity.Url.input_name * string
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Update of id
  | View_fields of id (** *)
  (** The type for container URL requests. *)

  val kind : t Kurl.kind
  (** The type for container URL request kinds. *)

  val v : t -> Kurl.t
  (** [v u] is [Kurl.v kind u]. *)

  (** {1:cons Convenience constructors} *)

  val res_name : container -> string
  (** [res_name c] is an URL path segment for naming [c]. *)

  val page : container -> Kurl.t
  (** [page c] is a {!Url.type-t.Page} URL request for [c]. *)
end
