(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Persons

    Contributors to references, either in author or editor position. *)

open Hyperbib.Std

(** {1:roles Person roles} *)

type role = Author | Editor (** *)
(** The type for person roles. *)

val role_type : role Type.t
(** [role_type] encodes roles in Ask values. *)

val role_to_string : role -> string
(** [role_to_string r] is an US-ASCII string represenation of [r]. *)

(** {1:persons Persons} *)

type id = Id.t
(** The type for person ids. These are allocated by the database. *)

type t
(** The type for persons. *)

type person = t
(** See {!t}. *)

val v :
  id:id -> last_name:string -> first_names:string ->
  orcid:string -> note:string -> private_note:string ->
  public:bool -> unit -> t
(** [v …] is a person with given attributes, see accessors for semantics. *)

val row : id -> string -> string -> string -> string -> string -> bool -> t
(** [row] is {!v} unlabelled. *)

val new' : t
(** [new'] is a new person. *)

val id : t -> id
(** [id p] is the unique identifier of [p].  *)

val last_name : t -> string
(** [last_name p] is the last name of [p]. *)

val first_names : t -> string
(** [first_names p] are the comma separated first names of [p]. *)

val orcid : t -> string
(** [orcid p] is the orcid of [p]. *)

val note : t -> string
(** [note p] is the public note of [p]. *)

val private_note : t -> string
(** [private_note p] is the private note of [p], only available
    to loged in users. *)

val public : t -> bool
(** [public p] is [true] iff [p]'s existence is made visible to the
    public. *)

(** {1:ops Operations} *)

val duplicate_data : t -> t
(** [duplicate_data s] is [s] with its last name altered and {!public} set to
    none. The {!id} is the one of [s]. *)

val created_equal : t -> t -> bool

(** {1:predcomp Predicates and comparisons} *)

val order_by_last_name : t -> t -> int
(** [order_by_name s0 s1] orders [s0] and [s1] in lexicographic
    order of their last name and then first names (we should likely
    let the DB do that). *)

(** {1:derived Derived data} *)

val names_lf : t -> string
(** [names_lf p] is the person's names, in last first order. *)

val names_fl : t -> string
(** [names_fl p] is the person's names, in first last order. *)

val index_letter : t -> char option
(** [index_letter p] is the letter under which [p] can be found
    in the index. {b FIXME} this should be made Unicode aware. *)

(** {1:tables Tables} *)

val id' : (t, id) Col.t
(** [id'] is the column for {!val-id}. *)

val last_name' : (t, string) Col.t
(** [last_name'] is the column for {!last_name}. *)

val first_names' : (t, string) Col.t
(** [first_names'] is the column for {!first_names}. *)

val orcid' : (t, string) Col.t
(** [orcid'] is the column for {!orcid}. *)

val note' : (t, string) Col.t
(** [note'] is the column for {!note}. *)

val private_note' : (t, string) Col.t
(** [private_note'] is the column for {!private_note}. *)

val public' : (t, bool) Col.t
(** [public'] is the column for {!public}. *)

val table : t Table.t
(** [table] is the person table in the database. *)

(** Person label applications. *)
module Label : Label.APPLICATION
  with type entity := t and type entity_id := id

(** {1:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id

val select : string Rel.value -> (t, Bag.unordered) Rel.Bag.t
val select_stmt : string -> t Sql.Stmt.t

val match' :
  last:string Rel.value -> first:string Rel.value -> orcid:string Rel.value ->
  (t, Bag.unordered) Rel.Bag.t

val match_stmt :
  last:string -> first:string -> orcid:string -> t Sql.Stmt.t

(** {1:url URLs} *)

(** Person URL requests *)
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
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
  | Replace of id
  | Replace_form of id
  | Input of
      Entity.Url.for_list * Entity.Url.input_name * role option * id
  | Input_create of
      Entity.Url.for_list * Entity.Url.input_name * role option * person
  | Input_finder of
      Entity.Url.for_list * Entity.Url.input_name * role option
  | Input_finder_find of
      Entity.Url.for_list * Entity.Url.input_name * role option * string
  | Update of id
  | View_fields of id (** *)
  (** The type for person URL requests. *)

  val kind : t Kurl.kind
  (** The type for person URL request kinds. *)

  val v : t -> Kurl.t
  (** [v u] is [Kulr.v kind u]. *)

  (** {1:cons Convenience constructors} *)

  val res_name : person -> string
  (** [res_name p] an URL path segment for naming [p]. *)

  val page : person -> Kurl.t
  (** [page p] is a {!type-Url.t.Page} URL request for [p]. *)
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
