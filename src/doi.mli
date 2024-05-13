(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** DOI names and their resolution.

    {b Note.} DOI names are ASCII case insensitive, we normalize
    them to lowercase, as it screams less and provides less problems
    for matching. *)

open Hyperbib_std

(** {1:dois DOI names} *)

type t = string
(** The type for DOI names. Note that this can be the empty string. On the
    empty string, resolutions errors. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a DOI name (without a resolver or [doi:]). *)

val normalize : t -> t
(** [normalize d] normalizes [d] by lowercasing US-ASCII characters. *)

(** {1:scraping Scraping} *)

val extract : ?start:int -> string -> t option
(** [extract s] looks for the first ["10."] prefix at or after [start]
    (defaults to [0]) and then tries to parse a DOI until the first
    {!Char.Ascii.is_blank} or the end of string.  The result is
    normalized.

    {b Note.} It seems that ISO 26324:2022 now allow other directory
    number than [10], this might need to be adjusted in the future. *)

val list_of_text_scrape : string -> t list
(** {list_of_text_scrape s] assuming an US-ASCII compatible text encoding
    roughly finds DOI names by looking for ["10."] and {!extract}ing them. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] is binary equality on DOI names. *)

val compare : t -> t -> int
(** [compare] is a total order on DOI names compatible with {!equal}. *)

(** {1:res Resolution} *)

val default_resolver : Webs_url.t
(** [default_resolver] is the default resolver used to resolve
    DOIs. This is {:https://doi.org}. *)

val resolve_to_url :
  ?resolver:string -> Webs.Http_client.t -> t ->
  (Webs_url.t, string) result
(** [resolve_to_url r ~resolver doi] resolve [doi] with [resolver]
      to an URL with [resolver] (defaults to {!default_resolver}). *)

val resolve_to_content_type :
  ?resolver:Webs_url.t -> content_type:string ->
  Webs.Http_client.t -> t -> (string option, string) result
(** [resolve_to_content_type ~resolver httpc ~content_type doi]
    resolves [doi] to a content_type [content_type] (the value of the
    HTTP [Accept:] header) with [resolver] (defaults to
    {!default_resolver}). [None] is returned if this results in a
    404. *)

val bibtex : string
(** [bibtex] is the BibTeX bibliographic format.
    This is ["application/x-bibtex; charset=utf-8"] *)

val formatted_citation : string
(** [formatted_citation] is ["text/x-bibliography; charset=utf-8"]. *)

val json : string
(** [json] is ["application/json; charset=utf-8"]. For crossref managed
    DOIs this seems to return the format {!Crossref}. *)
