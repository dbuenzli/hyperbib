(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00_http
open Result.Syntax

(* DOIs *)

type t = string
let pp = Fmt.string

let extract s =
  let s = String.trim s in
  match String.rindex_opt s '/' with
  | None -> s (* This is however unlikely to be a DOI. *)
  | Some i ->
      match String.rindex_from_opt s (i - 1) '/' with
      | None -> s
      | Some i -> String.subrange ~first:(i + 1) s

(* Resolving *)


let default_resolver = "https://doi.org"

let resp_success req resp = match Http.resp_status resp with
| 200 -> Ok (Some (Http.resp_body resp))
| 404 -> Ok None
| st ->
    Fmt.error "%s on %s: responded with %d"
      (Http.meth_to_string (Http.req_meth req)) (Http.req_uri req) st

let doi_uri ~resolver = function
| "" -> Fmt.error "DOI is empty"
| doi -> Ok (Fmt.str "%s/%s" resolver doi) (* FIXME URL encode ? *)

let resolve_to_uri ?(resolver = default_resolver) r doi =
  let* uri = doi_uri ~resolver doi in
  let req = Http.req ~uri `GET in
  let* resp = Httpr.perform ~follow:false r req in
  match List.assoc_opt "location" (Http.resp_headers resp) with
  | None -> Error "No 'location' header found in response"
  | Some uri -> Ok uri

let resolve_to_content_type
    ?(resolver = default_resolver) ~content_type r doi
  =
  let headers = ["Accept", content_type] in
  let* uri = doi_uri ~resolver doi in
  let req = Http.req ~headers ~uri `GET in
  let* resp = Httpr.perform r req in
  resp_success req resp

let bibtex = "application/x-bibtex; charset=utf-8"
let formatted_citation = "text/x-bibliography; charset=utf-8"
let json = "application/json; charset=utf-8"

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
