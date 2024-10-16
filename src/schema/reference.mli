(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** References. *)

open Hyperbib_std

(** {1:refs References} *)

(** The type for reference ids. These are allocated by the database. *)
module Id : Rel_kit.INT_ID

type t
(** The type for references. *)

val make :
  id:Id.t -> abstract:string -> container:Container.Id.t option ->
  created_ptime_s:float -> date:Date.partial option -> doi:Doi.t option ->
  isbn:string -> issue:string -> note:string -> pages:string ->
  private_note:string -> public:bool -> publisher:string -> title:string ->
  type':string -> volume:string -> t
(** [v] is a reference with given attributes. *)

val row :
  Id.t -> string -> Container.Id.t option -> float -> Date.year option ->
  Date.md_partial option -> Doi.t option -> string -> string -> string ->
  string -> string -> bool -> string -> string -> string -> string -> t
(** [row] is {!make} unlabelled. *)

val new' : t
(** [new'] is a new reference. *)

val id : t -> Id.t
(** [id] is the reference's id. *)

val container : t -> Container.Id.t option
(** [container_title] is the container title. *)

val created_ptime_s : t -> float
(** [created_ptime_s] is creation time as a POSIX timestamp. *)

val date : t -> Date.partial option
(** [date] is the publication date of the reference. *)

val doi : t -> Doi.t option
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

val id_map :
  Db.t -> 'a Rel_sql.Stmt.t -> ('a -> Id.t) -> ('a Id.Map.t, Db.error) result

(** {1:tables Tables and queries} *)

open Rel

val id' : (t, Id.t) Col.t
val abstract' : (t, string) Col.t
val created_ptime_s' : (t, float) Col.t
val container' : (t, Container.Id.t option) Col.t
val date_year' : (t, int option) Col.t
val date_md' : (t, Date.md_partial option) Col.t
val doi' : (t, Doi.t option) Col.t
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

(** {2:relations Relations}

    These definitions are here because we can't refer to [../] in OCaml. *)

type reference = t
(** See {!t}. *)

type reference_id = Id.t
(** See {!Id.t}. *)

type subject = Subject.t
(** See {!Subject.t} *)

type label = Label.t
(** See {!Label.t}. *)

(** Reference label applications. *)
module Label : Label.APPLICATION with type entity := t
                                  and type entity_id := Id.t

(** Contributor relation. *)
module Contributor : sig

  type t
  (** The type for reference contributor relation. *)

  val make :
    reference:Id.t -> person:Person.Id.t -> role:Person.Role.t ->
    position:int -> t
  (** [make ~reference ~person ~role ~position] is a contributor with
      given attributes. See accessors for semantics. *)

  val row : Id.t -> Person.Id.t -> Person.Role.t -> int ->  t
  (** [row] is unlabelled {!make}. *)

  val reference : t -> Id.t
  (** [reference] is the reference contributed to. *)

  val person : t -> Person.Id.t
  (** [person] is the person contributing. *)

  val role : t -> Person.Role.t
  (** [role] is the contribor's contribution role. *)

  val position : t -> int
  (** [position] is the contributor's position in the role list. *)

  val order_by_position : t -> t -> int

  (** {1:table Table} *)

  val reference' : (t, Id.t) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val person' : (t, Person.Id.t) Col.t
  (** [person'] is the column for {!person}. *)

  val role' : (t, Person.Role.t) Col.t
  (** [role'] is the column for {!val-role}. *)

  val position' : (t, int) Col.t
  (** [position'] is the column for {!position}. *)

  val table : t Table.t
  (** [table] is the contributor table. *)

  (** {1:queries Queries} *)

  val create : ?or_action:Rel_sql.insert_or_action -> t -> unit Rel_sql.Stmt.t
  (** [create c] creates a contribution. *)

  val of_ref_ids : (Id.t, 'a) Bag.t -> (t, Bag.unordered) Bag.t

  val persons :
    only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
    (Person.t, Bag.unordered) Bag.t
  (** [persons ~only_public cs] are the persons mentioned in [cs],
      No duplicates. *)

  val copy_contributions_stmt :
    src:Person.Id.t -> dst:Person.Id.t -> unit Rel_sql.Stmt.t

  val set_list :
    reference:Id.t -> authors:Person.Id.t list -> editors:Person.Id.t list ->
    (Db.t -> (unit, Db.error) result)
end

type subject_id = Subject.Id.t

(** Subject relation. *)
module Subject : sig

  type t
  (** The type for the reference subject relation. *)

  val make : reference:Id.t -> subject:Subject.Id.t -> t
  (** [make reference subject] indicates [reference] has subject [subject]. *)

  val row : Id.t -> Subject.Id.t -> t
  (** [row] is unabelled {!make}. *)

  val reference : t -> Id.t
  (** [reference s] is the subjected reference. *)

  val subject : t -> Subject.Id.t
  (** [subject s] is the subject. *)

  (** {1:table Table} *)

  val reference' : (t, Id.t) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val subject' : (t, Subject.Id.t) Col.t
  (** [person'] is the column for {!val-subject'}. *)

  val table : t Table.t
  (** The table for subject relationships. *)

  (** {1:queries Queries} *)

  val create : ?or_action:Rel_sql.insert_or_action -> t -> unit Rel_sql.Stmt.t
  (** [create sa] creates an author relationship. *)

  val of_ref_ids : (Id.t, 'a) Bag.t -> (t, Bag.unordered) Bag.t

  val subjects :
    only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
    (Subject.t, Bag.unordered) Bag.t
  (** [subjects apps] are the subjects mentioned by [apps].
      No duplicates. *)

  val filter :
    subjects:(Subject.t, 'a) Bag.t  ->
    (reference, 'a) Bag.t  -> (reference, Bag.unordered) Bag.t

  val filter_subject_id :
    Subject.Id.t Rel_query.value -> (reference, 'a) Bag.t ->
    (reference, Bag.unordered) Bag.t

  val ref_count_stmt : Subject.Id.t -> int Rel_sql.Stmt.t

  val copy_applications_stmt :
    src:Subject.Id.t -> dst:Subject.Id.t -> unit Rel_sql.Stmt.t

  val set_list : reference:Id.t -> Subject.Id.t list ->
    (Db.t -> (unit, Db.error) result)
end

(** Cites relation. *)
module Cites : sig

  type t
  (** The type for the reference subject relation. *)

  val make : reference:Id.t -> doi:Doi.t -> t
  (** [make reference doi] indicates [reference] cites document [doi]. *)

  val row : Id.t -> Doi.t -> t
  (** [row] is unlabelled {!make}.  *)

  val reference : t -> Id.t
  (** [reference s] is the subjected reference. *)

  val doi : t -> Doi.t
  (** [doi s] is the cited work. *)

  (** {1:table Table} *)

  val reference' : (t, Id.t) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val doi' : (t, Doi.t) Col.t
  (** [doi'] is the column for {!val-doi}. *)

  val table : t Table.t
  (** The table for cites relationships. *)

  (** {1:queries Queries} *)

  val create : t -> unit Rel_sql.Stmt.t
  (** [create sa] creates a cites relationship. *)

  val of_ref_ids : (Id.t, Bag.unordered) Bag.t -> (t, Bag.unordered) Bag.t

  val internal_of_ref_ids :
    (Id.t, Bag.unordered) Bag.t -> (Id.t * Id.t, Bag.unordered) Bag.t

(*  val internal_row : (Id.t * Id.t) Rel.Row.t *)

  val set_list :
    reference:Id.t -> dois:Doi.t list -> (Db.t -> (unit, Db.error) result)
end

(** Doc relation *)
module Doc : sig

  (** The type for reference ids. These are allocated by the database. *)
  module Id : Rel_kit.INT_ID

  type t
  (** The type for the reference document relation. *)

  val make :
    id:Id.t -> reference:reference_id -> blob_key:Blobstore.Key.text ->
    media_type:Media_type.t -> name:string -> origin:string -> public:bool -> t
  (** [make] is a new document see corresponding properties for semantics. *)

  val row :
    Id.t -> reference_id -> Blobstore.Key.text -> Media_type.t -> string ->
    string -> bool -> t
  (** [row] is unlabelled {!make}.  *)

  val id : t -> Id.t
  (** [id doc] is the document's id. *)

  val reference : t -> Id.t
  (** [reference s] is the reference to which the document is attached. *)

  val blob_key : t -> Blobstore.Key.text
  (** [blob_key s] is the key to the document's blob. *)

  val media_type : t -> Media_type.t
  (** [media_type d] is the media type of the document. *)

  val name : t -> string
  (** [name d] is a name for the document. If specified this is used for
      deriving a filename. *)

  val public : t -> bool
  (** [public d] indicates if the document can be made public. *)

  val origin : t -> string
  (** [origin d] is tne origin of the document. For example the DOI
      resolver that was used for looking it up. *)

  (** {1:table Table} *)

  val id' : (t, Id.t) Col.t
  (** [id'] is the column for {!val-id}. *)

  val reference' : (t, reference_id) Col.t
  (** [reference'] is the column for {!val-reference}. *)

  val blob_key' : (t, Blobstore.Key.text) Col.t
  (** [blob_key'] is the column for {!val-blob_key}. *)

  val media_type' : (t, Media_type.t) Col.t
  (** [media_type'] is the {!val-media_type} column. *)

  val name' : (t, string) Col.t
  (** [name] is the {!val-name} column. *)

  val origin' : (t, string) Col.t
  (** [origin'] is the {!val-origin} column. *)

  val public' : (t, bool) Col.t
  (** [public'] is the {!val-public} column. *)

  val table : t Table.t
  (** The table for document relationships. *)

  (** {1:queries Queries} *)

  val create : t -> unit Rel_sql.Stmt.t
  (** [create doc] creates a document relationship. *)

  include Entity.PUBLICABLE_QUERIES with type t := t and module Id := Id
end

(** {2:queries Queries} *)

include Entity.PUBLICABLE_QUERIES with type t := t and module Id := Id

val ids_of_refs : (t, 'a) Bag.t -> (Id.t, Bag.unordered) Bag.t
(** [ids_of_refs refs] are the identifiers of [refs]. *)

val containers_of_refs :
  only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
  (Container.t, Bag.unordered) Bag.t
(** [containers_of_refs refs] are the containers mentioned in [refs].
    No duplicates.*)

val filter_person_id :
  Person.Id.t Rel_query.value -> (t, 'a) Bag.t -> (t, Bag.unordered) Bag.t
(** [filter_person_id pid refs] are the elements of [refs] that
    are have the person identified by [pid] in author or editor position. *)

val filter_container_id :
  Container.Id.t Rel_query.value -> (t, 'a) Bag.t -> (t, Bag.unordered) Bag.t

val persons_public_ref_count_stmt : (Person.Id.t * int) Rel_sql.Stmt.t
val person_ref_count_stmt : Person.Id.t -> int Rel_sql.Stmt.t

val container_public_ref_count_stmt : (Container.Id.t * int) Rel_sql.Stmt.t
val container_ref_count_stmt : Container.Id.t -> int Rel_sql.Stmt.t

val subject_public_ref_count_stmt : (subject_id * int) Rel_sql.Stmt.t

val replace_container_stmt :
  this:Container.Id.t -> by:Container.Id.t -> unit Rel_sql.Stmt.t

val ids_citing_doi : Doi.t Rel_query.value -> (Id.t, Bag.unordered) Bag.t
val citing_doi : Doi.t Rel_query.value -> (t, Bag.unordered) Bag.t
val dois_cited : Id.t Rel_query.value -> (Doi.t, Bag.unordered) Bag.t
val find_dois : (Doi.t, 'a) Bag.t -> (t, Bag.unordered) Bag.t
val find_doi : Doi.t Rel_query.value -> (t, Bag.unordered) Bag.t

val author_ids_stmt : Id.t -> Person.Id.t Rel_sql.Stmt.t

(** {2:renderdata Render data} *)

type render_data =
  { list : t list;
    labels : label list Id.Map.t;
    authors : Person.t list Id.Map.t;
    containers : Container.t Container.Id.Map.t; (** Mapped by container id. *)
    editors : Person.t list Id.Map.t;
    subjects : subject list Id.Map.t;
    docs : Doc.t list Id.Map.t; }
(** The type for references render data. A list of references and their
    {{!relations}relations} sorted on reference ids. *)

val render_data :
  only_public:bool Rel_query.value -> (t, 'a) Bag.t ->
  (Db.t -> (render_data, Db.error) result)
(** [dataset refs] is a function that looks up a database for the
    the data needed to render [refs]. If [only_public] is [true]
    only public data is returned (this does not affect [refs]). *)

(** {1:urls URLs} *)

(** Reference URL requests. *)
module Url : sig

  (** {1:url_req URL requests} *)

  type named_id = string option * Id.t

  type t =
  | Change_authors_publicity of Id.t
  | Confirm_delete of Id.t
  | Create
  | Delete of Id.t
  | Doc of named_id * Doc.Id.t
(*
  | Duplicate of Id.t
  | Duplicate_form of Id.t
*)
  | Edit_form of Id.t
  | Fill_in_form of Doi.t
  | Index
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
(*  | Replace of Id.t
    | Replace_form of Id.t *)
  | Update of Id.t
  | View_fields of Id.t (** *)
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

  val doc : reference -> Doc.t -> Kurl.t
  (** [doc r doc] is a {!Url.type-t.Doc} URL request for [r] and [doc]. *)
end
