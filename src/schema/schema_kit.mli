(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Schema definition commonalities. *)

module Date_md_partial_rel : sig
  val t : Date.md_partial Rel.Type.t
  val v : Date.md_partial -> Date.md_partial Rel_query.value
  val equal :
    Date.md_partial Rel_query.value ->
    Date.md_partial Rel_query.value -> bool Rel_query.value

  val ( = ) :
    Date.md_partial Rel_query.value ->
    Date.md_partial Rel_query.value -> bool Rel_query.value
end

module Doi_rel : sig
  val t : Doi.t Rel.Type.t
  val v : Doi.t -> Doi.t Rel_query.value

  val equal :
    Doi.t Rel_query.value ->
    Doi.t Rel_query.value -> bool Rel_query.value

  val ( = ) :
    Doi.t Rel_query.value ->
    Doi.t Rel_query.value -> bool Rel_query.value
end

module Orcid_rel : sig
  val t : Orcid.t Rel.Type.t
  val v : Orcid.t -> Orcid.t Rel_query.value

  val equal :
    Orcid.t Rel_query.value ->
    Orcid.t Rel_query.value -> bool Rel_query.value

  val ( = ) :
    Orcid.t Rel_query.value ->
    Orcid.t Rel_query.value -> bool Rel_query.value
end
