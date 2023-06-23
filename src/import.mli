(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Data import functions. *)

open Hyperbib.Std

type 'a entity = [ `Exists of 'a | `To_create of 'a ]
(** The type for importing entities. Indicates whether it already
    exists or is to be created in the database. *)

module Doi : sig

  type ref
  (** The type for the metadata of a DOI. *)

  val ref_to_short_text_citation : ref -> string

  val get_ref :
    B0_http.Http_client.t option -> cache:Fpath.t -> Doi.t ->
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

val legacy :
  Db.t -> Hyperbib.Conf.t -> ((unit, string) result, Db.error) result

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
