(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Legacy data model and tools. *)

open Hyperbib.Std

module Page_url : sig
  type t =
  | Home
  | Help
  | Refs
  | Subjects
  | Persons
  | Years
  | Containers

  val kind : t Kurl.kind
end

(** {1:datamodel Data model} *)

module Person' : sig
  type t

  val v :
    family:string -> given:string option -> orcid:string option -> unit -> t
  (** [v ~name ~parent ~see_also ~description] is a description with
      given fields. *)

  val family : t -> string
  val given : t -> string option
  val orcid : t -> string option
  val anonymous : t

  val family_term : t -> string
  val given_term : t -> string
  val term : t -> string


  (** {1:mapset Sets and Maps} *)

  val uid : t -> int
  (** [uid p] is a unique id assigned by the program to any person
      value. This id is not stable accross program invocations. *)

  val compare : t -> t -> int
  (** [compare p0 p1] *)

  val migrate : t -> Person.t
end

(** Subjects *)
module Subject' : sig

  type t
  (** The type for subjects *)

  val v :
    name:string -> parent:string option -> see:string option ->
    see_also:string list -> description:string option -> t
  (** [v ~name ~parent ~see_also ~description] is a description with
      given fields. *)

  val name : t -> string
  val parent : t -> string option
  val see : t -> string option
  val see_also : t -> string list
  val description : t -> string option
  val compare_by_name : t -> t -> int

  (** {1:mapset Sets and Maps} *)

  val uid : t -> int
  (** [uid r] is a unique id assigned by the program to any subject value.
      This id is not stable accross program invocations. *)

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t

  (** {1:db Subject database} *)

  module Db : sig
    type subject = t
    type t
    val empty : t
    val add : t -> subject -> t
    val make_hierarchy : t -> t
    val find : string -> t -> subject list option
    val find_parent : subject -> t -> subject option
    val find_children : subject -> t -> subject list
    val find_see_also : subject -> t -> subject list
    val roots : t -> subject list
    val all : t -> subject list
  end

  val migrate : Db.t -> t -> Subject.t * Subject.See_also.t list
end

(** Containers. *)
module Container' : sig
  type t = string
  (** The type for containers. *)

  val migrate : t -> Container.t
end

(** References. *)
module Reference' : sig

  (** {1:refs References} *)

  type partial_date = int * (int * int option) option
  (** A partial date is a date of the form YYYY[-MM[-DD]]. *)

  type t
  (** The type for references. *)

  val v :
    authors:Person'.t list ->
    bibtex_src:Bibtex.t ->
    cites:Doi.t list ->
    container_title:string option ->
    date:partial_date option ->
    doi:Doi.t option ->
    editors:Person'.t list ->
    issue:string option ->
    type':string ->
    note:string option ->
    pages:string option ->
    publisher:string option ->
    subjects:string list ->
    title:string option ->
    volume:string option ->
    t
  (** [v] constructs a reference. See the accessors for semantics. *)

  (** {1:refs Fields a} *)

  val authors : t -> Person'.t list
  (** [authors] are the authors. *)

  val bibtex_src : t -> Bibtex.t
  (** [bib] is a potentional source BibT{_E}X entry. *)

  val container_title : t -> string option
  (** [container_title] is the container title. *)

  val date : t -> partial_date option
  (** [date] is the publication date of the reference. *)

  val doi : t -> Doi.t option
  (** [doi] is the DOI name of the reference. *)

  val editors : t -> Person'.t list
  (** [editors] is the list of editors. *)

  val issue : t -> string option
  (** [issue] is the issue number. *)

  val type' : t -> string
  (** [type'] is the type of work. *)

  val note : t -> string option
  (** [note] is the reference's note. *)

  val publisher : t -> string option
  (** [publisher] is the publisher. *)

  val pages : t -> string option
  (** [pages] are the pages. *)

  val cites : t -> Doi.t list
  (** [cites] are the DOIs of referenced works. *)

  val subjects : t -> string list
  (** [subjects] are the subjects. *)

  val title : t -> string option
  (** [title] is the reference title. *)

  val volume : t -> string option
  (** [volume] is the volume. *)

  (** {1:derived Derived} *)

  val year : t -> int
  (** [year r] is [r]'s year. If the information is not available
      this is [0]. *)

  val compare_by_date : t -> t -> int

  (** {1:mapset Sets and Maps} *)

  val uid : t -> int
  (** [uid r] is a unique id assigned by the program to any ref value.
      This id is not stable accross program invocations. *)

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t

  val migrate : Container.t list -> t -> Reference.t

  type migrate_person_map = Person.t B00_std.String.Map.t
  type migrate_subject_map = Subject.t B00_std.String.Map.t
  val migrate_person_map : Person.t list -> migrate_person_map
  val migrate_subject_map : Subject.t list -> migrate_subject_map
  val migrate_contributors :
    migrate_person_map -> t -> Reference.Contributor.t list
  val migrate_subjects : migrate_subject_map -> t -> Reference.Subject.t list
  val migrate_cites : t -> Reference.Cites.t list
end

(** Data for the database in JSON tables as given by office 385 *)
module Data_tables : sig
  open B00_serialk_json

  val refs_to_bibs : file:Fpath.t -> (Bibtex.t list, string) result
  val subjs_to_subjects : file:Fpath.t ->
    (Subject'.t list, string) result
end


(** Database of references. *)
module Refdb : sig
  type error = [ `Unknown_subjects of Reference'.t * string list ]

  type t
  (** The type for databases. *)

  val empty : Subject'.Db.t -> t
  (** [empty subject] is the empty database using subjects [subjects]. *)

  val subjects : t -> Subject'.Db.t

  (** {1:refs References} *)

  val add : t -> Reference'.t -> t
  (** [add db r] is [db] with reference [r] added. *)

  val refs : t -> Reference'.t list
  (** [refs db] is the list of references in the database. *)

  val citations : t -> Reference'.t -> Reference'.t list
  (** [citations t r] are the documents cited by [r] that are also in [db]. *)

  val container_title_index : t -> string list
  (** [container_title_index db] are the container titles mentioned by
      the references of [db]. *)

  val find_container_title : t -> string -> Reference'.t list
  (** [find_container_title db t] are the references of [db] with
      container title [t]. *)

  val person_index : t -> Person'.t list
  (** [person_index db] are the persons mentioned by the references of [db]. *)

  val find_person : t -> Person'.t -> Reference'.t list
  (** [find_person db p] are the references of [db] with person [p]. *)

  val subject_index : t -> Subject'.t list
  (** [subject db] are the subjects mentioned by the references of [db]. *)

  val find_subject : t -> Subject'.t -> Reference'.t list
  (** [find_subject db s] are the references with subject [s]. *)

  val year_index : t -> int list
  (** [year_index db] are the years mentioned by the references of [db]. *)

  val find_year : t -> int -> Reference'.t list
  (** [find_year db y] are the references of [db] published in year [y]. *)
end

val ref_of_bib_and_doi_meta :
  Doi.t -> Bibtex.t -> B00_serialk_json.Json.t -> (Reference'.t, string) result

val refdb_of_tables : B00_std.Fpath.t -> (Refdb.t, string) result
val import : Hyperbib.Data_conf.t -> B00_std.Os.Exit.t

(** {1:tools Tools} *)

val gen_csv : Reference'.t list -> (string, 'a) result

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
