(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** DOIs names and their resolution. *)

open B00_std

(** {1:dois DOIs} *)

type t = string
(** The type for DOIs. Note that this can be the empty string. On the
    empty string, resolutions errors. *)

val pp : t Fmt.t
(** [pp] formats a DOI. *)

val extract : string -> t
(** [extract s] tries to remove any resolver prefix of [s].
    It just cuts the string at the last to one '/' character or
    returns [s] otherwise. *)

(** {1:res Resolution} *)

val default_resolver : B00_http.Uri.t
(** [default_resolver] is the default resolver used to
    resolve DOIs. This is [https://doi.org]. *)

val resolve_to_uri :
  ?resolver:string -> B00_http.Httpr.t -> t -> (B00_http.Uri.t, string) result
(** [resolve_to_uri r ~resolver doi] resolve [doi] with [resolver]
      to an URI with [resolver] (defaults to {!default_resolver}). *)

val resolve_to_content_type :
  ?resolver:B00_http.Uri.t -> content_type:string ->
  B00_http.Httpr.t -> t -> (string option, string) result
(** [resolve_to_bib r ~resolver ~format doi] resolves [doi] to a
    format [format] (the value of the HTTP [Accept:] header)
    with [resolver] (defaults to {!default_resolver}). [None] is returned
    if this results in a 404. *)

val bibtex : string
(** [bibtex] is the BibTeX bibliographic format.
    This is ["application/x-bibtex; charset=utf-8"] *)

val formatted_citation : string
(** [formatted_citation] is ["text/x-bibliography; charset=utf-8"]. *)

val json : string
(** [json] is ["application/json; charset=utf-8"]. For crossref managed
    DOIs this seems to return the format {!Crossref}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

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
