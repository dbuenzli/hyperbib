(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** References. *)

open Hyperbib.Std

(** {1:refs References} *)

type id = Id.t
(** The type for reference ids. These are allocated by the database. *)

type t
(** The type for references. *)

type reference = t
(** See {!t}. *)

val v :
  id:id -> abstract:string -> container:Container.id option ->
  date:Date.partial option -> doi:Doi.t -> isbn:string -> issue:string ->
  note:string -> pages:string -> private_note:string ->
  public:bool -> publisher:string -> title:string -> type':string ->
  volume:string -> t
(** [v] is a reference with given attributes. *)

val row :
  id -> string -> Container.id option -> Date.year option ->
  Date.md_partial option -> Doi.t -> string -> string -> string -> string ->
  string -> bool -> string -> string -> string -> string -> t
(** [row] is {!v} unlabelled. *)

val new' : t
(** [new'] is a new reference. *)

val id : t -> Id.t
(** [id] is the reference's id. *)

val container : t -> Container.id option
(** [container_title] is the container title. *)

val date : t -> Date.partial option
(** [date] is the publication date of the reference. *)

val doi : t -> Doi.t
(** [doi] is the DOI name of the reference. *)

val isbn : t -> string
(** [isbn] is the ISBN of the reference. *)

val issue : t -> string
(** [issue] is the issue number. *)

val note : t -> string
(** [note] is the reference's note. *)

val pages : t -> string
(** [pages] are the pages. *)

val private_note : t -> string
(** [private_note] is the reference's private note. *)

val public : t -> bool
(** [public] is the publication status of the reference. *)

val publisher : t -> string
(** [publisher] is the publisher. *)

val title : t -> string
(** [title] is the reference title. *)

val type' : t -> string
(** [type'] is the type' of work. *)

val volume : t -> string
(** [volume] is the volume. *)

(** {1:derived Derived} *)

val is_monograph_part : t -> bool
(** [is_monograph_part r] is [true] iff [r] is part of a book
    or proceedings. *)

val year : t -> int
(** [year r] is [r]'s year. If the information is not available this is [0]. *)

val non_empty_title : t -> string
(** [non_empty_title r] is [r]'s title or {!Uimsg.untitled} if it
    is empty. *)

val compare_by_date : t -> t -> int
(** [compare_by_date r0 r1] orders [r0] and [r1] by increasing date. *)

type render_data =
  { list : t list;
    labels : Label.t list Id.Map.t;
    authors : Person.t list Id.Map.t;
    containers : Container.t Id.Map.t; (** Mapped by container id. *)
    editors : Person.t list Id.Map.t;
    subjects : Subject.t list Id.Map.t; }
(** The type for a references render data. A list of references and their
    {{!relation}relations} sorted on reference ids. This needs to be defined
    here to avoid module name mixups. See {!val-render_data}. *)

(** {1:tables Tables and queries} *)

open Rel

val id' : (t, id) Col.t
val abstract' : (t, string) Col.t
val container' : (t, Container.id option) Col.t
val date_year' : (t, int option) Col.t
val date_md' : (t, Date.md_partial option) Col.t
val doi' : (t, Doi.t) Col.t
val isbn' : (t, string) Col.t
val issue' : (t, string) Col.t
val note' : (t, string) Col.t
val pages' : (t, string) Col.t
val private_note' : (t, string) Col.t
val public' : (t, bool) Col.t
val publisher' : (t, string) Col.t
val title' : (t, string) Col.t
val type'' : (t, string) Col.t
val volume' : (t, string) Col.t
val table : t Table.t


val col_values_for_date : Date.partial option -> t Col.value * t Col.value

(** {2:relations Relations} *)

(** Reference label applications. *)
module Label : Label.APPLICATION with type entity := t and type entity_id := id

(** Contributor relation. *)
module Contributor : sig

  type t
  (** The type for reference contributor relation. *)

  val v :
    reference:id -> person:Person.id -> role:Person.role -> position:int -> t
  (** [v ~reference ~person ~role ~position] is a contributor with
      given attributes. See accessors for semantics. *)

  val row : id -> Person.id -> Person.role -> int ->  t
  (** [row] is unlabelled {!v}. *)

  val reference : t -> id
  (** [reference] is the reference contributed to. *)

  val person : t -> Person.id
  (** [person] is the person contributing. *)

  val role : t -> Person.role
  (** [role] is the contribor's contribution role. *)

  val position : t -> int
  (** [position] is the contributor's position in the role list. *)

  val order_by_position : t -> t -> int

  (** {1:table Table} *)

  val reference' : (t, id) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val person' : (t, Person.id) Col.t
  (** [person'] is the column for {!person}. *)

  val role' : (t, Person.role) Col.t
  (** [role'] is the column for {!val-role}. *)

  val position' : (t, int) Col.t
  (** [position'] is the column for {!position}. *)

  val table : t Table.t
  (** [table] is the contributor table. *)

  (** {1:queries Queries} *)

  val create : ?or_action:Rel_sql.insert_or_action -> t -> unit Rel_sql.Stmt.t
  (** [create c] creates a contribution. *)

  val of_ref_ids : (id, 'a) Bag.t -> (t, Bag.unordered) Bag.t

  val persons :
    only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
    (Person.t, Bag.unordered) Bag.t
  (** [persons ~only_public cs] are the persons mentioned in [cs],
      No duplicates. *)

  val copy_contributions_stmt :
    src:Person.id -> dst:Person.id -> unit Rel_sql.Stmt.t

  val set_list :
    reference:id -> authors:Person.id list -> editors:Person.id list ->
    (Db.t -> (unit, Db.error) result)
end

(** Subject relation. *)
module Subject : sig

  type t
  (** The type for the reference subject relation. *)

  val v : reference:id -> subject:Subject.id -> t
  (** [v reference subject] indicates [reference] has subject [subject]. *)

  val row : id -> Person.id -> t
  (** [row] is unabelled {!v}. *)

  val reference : t -> id
  (** [reference s] is the subjected reference. *)

  val subject : t -> Subject.id
  (** [subject s] is the subject. *)

  (** {1:table Table} *)

  val reference' : (t, id) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val subject' : (t, id) Col.t
  (** [person'] is the column for {!val-subject'}. *)

  val table : t Table.t
  (** The table for subject relationships. *)

  (** {1:queries Queries} *)

  val create : ?or_action:Rel_sql.insert_or_action -> t -> unit Rel_sql.Stmt.t
  (** [create sa] creates an author relationship. *)

  val of_ref_ids : (id, 'a) Bag.t -> (t, Bag.unordered) Bag.t

  val subjects :
    only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
    (Subject.t, Bag.unordered) Bag.t
  (** [subjects apps] are the subjects mentioned by [apps].
      No duplicates. *)

  val filter :
    subjects:(Subject.t, 'a) Bag.t  ->
    (reference, 'a) Bag.t  -> (reference, Bag.unordered) Bag.t

  val filter_subject_id :
    Subject.id Rel_query.value -> (reference, 'a) Bag.t ->
    (reference, Bag.unordered) Bag.t

  val ref_count_stmt : Subject.id -> int Rel_sql.Stmt.t

  val copy_applications_stmt :
    src:Subject.id -> dst:Subject.id -> unit Rel_sql.Stmt.t

  val set_list : reference:id -> Subject.id list ->
    (Db.t -> (unit, Db.error) result)
end

(** Cites relation. *)
module Cites : sig

  type t
  (** The type for the reference subject relation. *)

  val v : reference:id -> doi:Doi.t -> t
  (** [v reference doi] indicates [reference] cites document [doi]. *)

  val row : id -> Doi.t -> t
  (** [row] is unlabelled {!v}.  *)

  val reference : t -> id
  (** [reference s] is the subjected reference. *)

  val doi : t -> Doi.t
  (** [doi s] is the cited work. *)

  (** {1:table Table} *)

  val reference' : (t, id) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val doi' : (t, Doi.t) Col.t
  (** [doi'] is the column for {!val-doi}. *)

  val table : t Table.t
  (** The table for cites relationships. *)

  (** {1:queries Queries} *)

  val create : t -> unit Rel_sql.Stmt.t
  (** [create sa] creates a cites relationship. *)

  val of_ref_ids : (id, Bag.unordered) Bag.t -> (t, Bag.unordered) Bag.t

  val internal_of_ref_ids :
    (id, Bag.unordered) Bag.t -> (id * id, Bag.unordered) Bag.t

  val internal_row : (id * id) Rel.Row.t

  val set_list :
    reference:id -> dois:Doi.t list -> (Db.t -> (unit, Db.error) result)
end

(** {2:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and type id := id

val ids_of_refs : (t, 'a) Bag.t -> (id, Bag.unordered) Bag.t
(** [ids_of_refs refs] are the identifiers of [refs]. *)

val containers_of_refs :
  only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
  (Container.t, Bag.unordered) Bag.t
(** [containers_of_refs refs] are the containers mentioned in [refs].
    No duplicates.*)

val filter_person_id :
  Person.id Rel_query.value -> (t, 'a) Bag.t -> (t, Bag.unordered) Bag.t
(** [filter_person_id pid refs] are the elements of [refs] that
    are have the person identified by [pid] in author or editor position. *)

val filter_container_id :
  Container.id Rel_query.value -> (t, 'a) Bag.t -> (t, Bag.unordered) Bag.t

val persons_public_ref_count_stmt : (Person.id * int) Rel_sql.Stmt.t
val person_ref_count_stmt : Person.id -> int Rel_sql.Stmt.t

val container_public_ref_count_stmt : (Container.id * int) Rel_sql.Stmt.t
val container_ref_count_stmt : Container.id -> int Rel_sql.Stmt.t

val subject_public_ref_count_stmt : (int * int) Rel_sql.Stmt.t

val replace_container_stmt :
  this:Container.id -> by:Container.id -> unit Rel_sql.Stmt.t

val ids_citing_doi : Doi.t Rel_query.value -> (id, Bag.unordered) Bag.t
val citing_doi : Doi.t Rel_query.value -> (t, Bag.unordered) Bag.t
val dois_cited : id Rel_query.value -> (Doi.t, Bag.unordered) Bag.t
val find_dois : (Doi.t, 'a) Bag.t -> (t, Bag.unordered) Bag.t
val find_doi : Doi.t Rel_query.value -> (t, Bag.unordered) Bag.t

val author_ids_stmt : id -> Person.id Rel_sql.Stmt.t

(** {2:renderdata Render data} *)

val render_data :
  only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
  (Db.t -> (render_data, Db.error) result)
(** [dataset refs] is a function that looks up a database for the
    the data needed to render [refs]. If [only_public] is [true]
    only public data is returned (this does not affect [refs]). *)

(** {1:urls URLs} *)

(** Container URL requests. *)
module Url : sig

  (** {1:url_req URL requests} *)

  type named_id = string option * id

  type t =
  | Change_authors_publicity of id
  | Confirm_delete of id
  | Create
  | Delete of id
(*
  | Duplicate of id
  | Duplicate_form of id
*)
  | Edit_form of id
  | Fill_in_form of Doi.t
  | Index
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
(*  | Replace of id
    | Replace_form of id *)
  | Update of id
  | View_fields of id (** *)
  (** The type for reference URL requests. *)

  val kind : t Kurl.kind
  (** [kind] is the kind of reference URL requests. *)

  val v : t -> Kurl.t
  (** [v u] is [Kurl.v kind u]. *)

  (** {1:conv Convenience constructors} *)

  val res_name : reference -> string
  (** [res_name r] is an URL path segment for naming [r]. *)

  val page : reference -> Kurl.t
  (** [page r] is a {!Url.type-t.Page} URL request for [r]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

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
