(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Labels. *)

open Hyperbib.Std
open Rel

(** {1:labels Labels} *)

type id = Id.t
(** The type for label ids. *)

type color = int64
(** The type for sRGBA colors with 16-bits per components. *)

type name = string
(** The type for label names. *)

type t
(** The type for labels *)

val v :
  id:id -> name:name -> synopsis:string -> color:color ->
  note:string -> private_note:string -> public:bool -> t
(** [v] constructs a label with the given attributes, see accessors
    for semantics. *)

val row : id -> name -> string -> color -> string -> string -> bool -> t
(** [row] is {!v} unlaballed. *)

val id : t -> id
(** [id l] is the label identifier. *)

val name : t -> name
(** [name l] is the label name. *)

val synopsis : t -> string
(** [synopsis l] is the label one sentence description. *)

val color : t -> color
(** [color l] is the label color. *)

val note : t -> string
(** [note l]  is the label public note. *)

val private_note : t -> string
(** [private_note l] is the label private note. *)

val public : t -> bool
(** [public l] is the label publication status. *)

val order_by_name : t -> t -> int

(** {1:table Table and queries} *)

val id' : (t, id) Col.t
(** [id'] is the column for {!val-id}. *)

val name' : (t, name) Col.t
(** [name'] is the column for {!val-name}. *)

val synopsis' : (t, string) Col.t
(** [synopsis'] is the column for {!synopsis}. *)

val color' : (t, color) Col.t
(** [color'] is the column for {!val-color}. *)

val note' : (t, string) Col.t
(** [note'] is the column for {!note}. *)

val private_note' : (t, string) Col.t
(** [private_note'] is the column for {!private_note}. *)

val public' : (t, bool) Col.t
(** [public'] is the column for {!public}. *)

val table : t Table.t
(** [table] is the label table. *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id

(** {1:labeling Labeling entities} *)

type label_id = id
type label = t

(** The type for label application relations. *)
module type APPLICATION = sig

  (** {1:applications Applications} *)

  type entity
  (** The type for labeled entities. *)

  type entity_id = Id.t
  (** The type for labeled entity identifiers. *)

  type t
  (** The type for relating entites and labels. *)

  val v : entity:entity_id -> label:label_id -> t
  (** [v entity label] applies label [label] to the entity identified by
      [entity]. *)

  val row : entity_id -> id -> t
  (** [row] is {!v} unlabelled. *)

  val entity : t -> entity_id
  (** [entity a] is the entity to which the label is applied. *)

  val label : t -> label_id
  (** [label a] is the applied label. *)

  (** {1:table Table and queries} *)

  val entity' : (t, entity_id) Col.t
  (** [entity'] is the column for {!val-entity}. *)

  val label' : (t, label_id) Col.t
  (** [label'] is the column for {!val-label}. *)

  val table : t Table.t
  (** [table] is the table for label applications. *)

  val create : t -> unit Rel_sql.Stmt.t
  val applications : (entity_id, 'a) Bag.t -> (t, Bag.unordered) Bag.t
  val of_applications :
    only_public:bool Rel_query.value ->
    (t, 'a) Bag.t -> (label, Bag.unordered) Bag.t

  val copy_applications_stmt :
    src:entity_id -> dst:entity_id -> unit Rel_sql.Stmt.t
end

(** [For_entity (E)] is a relation to apply labels to entity [E]. *)
module For_entity (E : Entity.IDENTIFIABLE) :
  (APPLICATION with type entity := E.t and type entity_id := id)

(** {1:url Url} *)

(** Label URL requests. *)
module Url : sig
  type label = t
  type named_id = string option * id
  type t =
  | Create
  | Edit of id
  | Edit_new of { cancel : string option }
  | Index
  | Page of named_id
  | Update of id
  | View of id

  val kind : t Kurl.kind
  val v : t -> Kurl.t

  (** {1:cons Convenience constructors} *)

  val res_name : label -> string
  (** [res_name l] an URL path segment name the resource representing [l]. *)

  val page : label -> Kurl.t
  (** [page l] is a {!Page} URL request for [l]. *)
end
