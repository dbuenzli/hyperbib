(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data import functions. *)

open Hyperbib_std

type 'a entity = [ `Exists of 'a | `To_create of 'a ]
(** The type for importing entities. Indicates whether it already
    exists or is to be created in the database. *)

module Doi : sig

  type ref
  (** The type for the metadata of a DOI. *)

  val ref_to_short_text_citation : ref -> string

  val get_ref :
    Http_client.t option -> cache:Fpath.t -> Doi.t ->
    (ref option, string) result

  val cites_of_ref : ref -> Doi.t list

  val get_container :
    create_public:bool -> Db.t -> ref ->
    (Container.t entity option, Db.error) result

  val reference_of_ref :
    ?note:string -> public:bool -> container_id:int option -> ref ->
    Reference.t

  val authors_editors_of_ref :
    create_public:bool -> Db.t -> ref ->
    (Person.t entity list * Person.t entity list, Db.error) result
end
