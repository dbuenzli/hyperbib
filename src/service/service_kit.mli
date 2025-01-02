(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Service commonalities. *)

open Hyperbib_std

val find_dupe_doi :
  ?suggestion_dupe_check:bool -> Page.Gen.t -> self:Kurl.t -> Db.t ->
  Doi.t -> (El.html option, Http.Response.t) result

val lookup_doi :
  Service_env.t -> Doi.t ->
  (Doi.t * (Import.Doi.ref option, string) result, Http.Response.t) result

val empty_reference_form :
  ?msg:El.html -> Page.Gen.t -> self:Kurl.t ->
  cancel:Entity.Url.cancel_url -> El.html

val fill_in_reference_form :
  ?suggestion_dupe_check:bool->
  Service_env.t -> Db.t -> self:Kurl.t -> cancel:Entity.Url.cancel_url ->
  doi:string -> (string option * El.html, Http.Response.t) result
