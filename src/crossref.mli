(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CrossRef metadata.

    Partial modelling of the format described
    {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md}
    here} with [Jsont]. *)

open Hyperbib_std

(** {1:data Data} *)

type partial_date = int * (int * int option) option
(** The type for
    {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#partial-date}partial date}. *)

val partial_date_jsont : partial_date Jsont.t

(** {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#contributor}Contributor} objects. *)
module Contributor : sig
  type t =
    { family : string;
      given : string;
      orcid : Orcid.t option }

  val jsont : t Jsont.t
  val equal : t -> t -> bool
end

(** {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#reference}Reference} objects. *)
module Reference : sig
  type t = { doi : Doi.t option }
  val jsont : t Jsont.t
end

(** {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#license}License} objects. *)
module License : sig
  type t = {
    content_version : string;
    start : partial_date;
    url : string; }

  val jsont : t Jsont.t
end

(** {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#resource-link}Ressource link} objects. *)
module Ressource_link : sig
  type t =
    { intended_application : string;
      content_version : string;
      url : string;
      content_type : string option; }

  val jsont : t Jsont.t
end


(** {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#work}
    Work} objects. *)
module Work : sig
  type t =
    { author : Contributor.t list;
      abstract : string;
      container_title : string list;
      doi : Doi.t;
      editor : Contributor.t list;
      issn : string list;
      isbn : string list;
      issue : string option;
      issued : partial_date;
      license : License.t list;
      link : Ressource_link.t list;
      page : string option;
      publisher : string;
      reference : Reference.t list;
      subject : string list;
      title : string list;
      type' : string;
      volume : string option; }

  val jsont : t Jsont.t
end

(** {1:lookup Lookup} *)

val for_doi :
  Webs.Http_client.t option ->
  cache:Fpath.t -> Doi.t -> (Work.t option, string) result
(** [for_doi httpc cache doi] looks up crossref metadata for DOI
    [doi].  Looks up in the local [cache] first. If [httpc] is [None]
    looked up in the cache only. [None] is returned if the DOI cannot
    be resolved (404). *)

(** {1:types Types}

    From {:https://api.crossref.org/v1/types} *)

val types : (string * string) list
