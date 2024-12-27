(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** DOI names and their resolution.

    {b Note.} DOI names are ASCII case insensitive, we normalize them
    to lowercase. It screams less and provides less problems for
    matching. *)

open Hyperbib_std

(** {1:dois DOI names} *)

type t = string
(** The type for DOI names. {b TODO} make abstract. *)

(** {1:scraping Scraping} *)

val normalize : string -> string
(** [normalize s] normalizes [s] by lowercasing US-ASCII characters. *)

val extract : ?start:int -> string -> t option
(** [extract s] looks for the first ["10."] prefix at or after [start]
    (defaults to [0]) and then tries to delimit a DOI:
    {ul
    {- If ["10."] starts at index [0], it simply takes all bytes until
       the first the first {!Char.Ascii.is_white} or
       {!Char.Ascii.is_control} or the end of string.}
    {- Otherwise it assumes DOIs have balanced bracket delimiters and
       stops on unbalanced brackets ([')'], ['\]'], ['}'], ['>']). It
       also removes a final quote or double quote if the context looks
       markupish.}}

    {b Note.} It seems that ISO 26324:2022 now allow other directory
    number than [10], this might need to be adjusted in the future. *)

val fold_text_scrape : (t -> 'acc -> 'acc) -> string -> 'acc -> 'acc
(** [fold_text_scrape f s acc] folds over the DOIs in [s] with [f]
    starting with [acc]. This assumes an US-ASCII compatible text encoding
    and repeatedly find them with {!extract}. *)

(** {1:convert Converting} *)

(**/**)
val unsafe_of_string : string -> t
(**/**)

val of_string : string -> (t, string) result
(** [of_string s] is a DOI from [s]. This uses {!extract} but
    assumes the string was already delimited. *)

val to_string : t -> string
(** [to_string d] is [d] as a string without a resolver or [doi:].
    See also {!presentations}. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a DOI name like {!to_string} does. *)

(** {1:presentations Presentations} *)

val default_resolver : Webs.Url.t
(** [default_resolver] is the default resolver used to resolve
    DOIs. This is {:https://doi.org}. *)

val as_uri : t -> string
(** [as_uri d] is [d] prefixed by [doi:]. This percent encodes [d]
    as needed. *)

val as_url : ?resolver:Webs.Url.t -> t -> Webs.Url.t
(** [as_url ~resolver d] is [d] prefixed by [resolver] (defaults to
    {!default_resolver}). This precent encodes [d] as needed. *)

val as_filename : t -> string
(** [as_filename d] is [d] as a file name. It simply munges the
    slashes and backlashes into [_]. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] is binary equality on DOI names. *)

val compare : t -> t -> int
(** [compare] is a total order on DOI names compatible with {!equal}. *)

(** {1:sets_maps Sets and Maps} *)

module Set : Set.S with type elt := t
(** Sets of DOIs. *)

module Map : Map.S with type key := t
(** Maps of DOIs. *)

(** {1:res Resolution} *)

val resolve_to_url :
  ?resolver:string -> Webs.Http_client.t -> t ->
  (Webs.Url.t, string) result
(** [resolve_to_url r ~resolver doi] resolve [doi] with [resolver]
      to an URL with [resolver] (defaults to {!default_resolver}). *)

val resolve_to_content_type :
  ?resolver:Webs.Url.t -> content_type:string ->
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

val to_document :
  ?url_only:bool -> ?resolver:Webs.Url.t -> Webs.Http_client.t ->
  media_type:Webs.Media_type.t -> t->
  (Webs.Url.t * Webs.Http.Body.t, string) result
(** [to_document] is a best-effort resolver to try find a document of
    type [media_type] using [resolver]. It tries to resolve directly
    to the given [media_type] or if a webpage is returned tries to
    scrape it for a link to the document using various heuristics. In
    case of success returns an URL to the document and a body to its content.
    If [only_url] is specified the body is {!Webs.Http.Body.empty}. *)
