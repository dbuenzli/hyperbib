(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** DOI resolution UI fragments *)

open Hyperbib_std

val warn_doi_exists :
  Page.Gen.t -> self:Kurl.t -> Doi.t -> Reference.t -> El.html

val warn_doi_suggestion_exists :
  Page.Gen.t -> self:Kurl.t -> Doi.t -> Suggestion.t -> El.html
