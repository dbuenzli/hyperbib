(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [Jsonq] queries for JSON CrossRef metadata.

    The format is described
    {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md}
    here}. This a partial modelling of the format. *)

open Hyperbib.Std
open B0_json

type partial_date = int * (int * int option) option
(** The type for {{:https://github.com/Crossref/rest-api-doc/blob/master/api_format.md#partial-date}partial date}. *)

val partial_date : partial_date Jsonq.t
(** [partial_date] queries a partial date. *)

(** Query contributor objects. *)
module Contributor : sig
  val family : string Jsonq.t
  val given : string option Jsonq.t
  val orcid : string option Jsonq.t
end

(** Query reference objects. *)
module Reference : sig
  val doi : string option Jsonq.t
end

(** Query work objects. *)
module Work : sig
  val author : 'a Jsonq.t -> 'a list option Jsonq.t
  val abstract : string option Jsonq.t
  val container_title : string list option Jsonq.t
  val doi : string Jsonq.t
  val editor : 'a Jsonq.t -> 'a list option Jsonq.t
  val issn : string list option Jsonq.t
  val isbn : string list option Jsonq.t
  val issue : string option Jsonq.t
  val issued : partial_date Jsonq.t
  val page : string option Jsonq.t
  val publisher : string Jsonq.t
  val reference : 'a Jsonq.t -> 'a list option Jsonq.t
  val subject : string list option Jsonq.t
  val title : string list Jsonq.t
  val type' : string Jsonq.t
  val volume : string option Jsonq.t
end

val for_doi :
  B0_http.Http_client.t option ->
  cache:Fpath.t -> Doi.t -> (B0_json.Json.t option, string) result
(** [for_doi httpr cache doi] looks up crossref metadata for doi [doi].
    Looks up in the local [cache] first. [None] is returned if the
    doi cannot be resolved (404). *)

(** {1:types Types}

    From https://api.crossref.org/v1/types *)

val types : (string * string) list

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
