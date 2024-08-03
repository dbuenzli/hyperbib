(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_http
open Result.Syntax

(* DOIs *)

type t = string
let pp = Fmt.string

let normalize = String.lowercase_ascii
let extract s =
  let s = String.trim s in
  match String.find_sub ~sub:"10." s with
  | None -> normalize s
  | Some first -> normalize (String.subrange ~first s)

(* Resolving *)

let default_resolver = "https://doi.org"

let response_success request response = match Http.Response.status response with
| 200 -> Ok (Some (Http.Response.body response))
| 404 -> Ok None
| status ->
    let method' = Http.method_to_string (Http.Request.method' request) in
    let url = Http.Request.url request in
    Fmt.error "%s on %s: responded with %d" method' url status

let doi_url ~resolver = function
| "" -> Fmt.error "DOI is empty"
| doi -> Ok (Fmt.str "%s/%s" resolver (Webs.Http.Pct.encode `Uri doi))

let resolve_to_url ?(resolver = default_resolver) httpc doi =
  let* url = doi_url ~resolver doi in
  let request = Http.Request.make ~url `GET in
  let* response = Http_client.fetch ~follow:false httpc request in
  match List.assoc_opt "location" (Http.Response.headers response) with
  | None -> Error "No 'location' header found in response"
  | Some uri -> Ok uri

let resolve_to_content_type
    ?(resolver = default_resolver) ~content_type httpc doi
  =
  let headers = ["Accept", content_type] in
  let* url = doi_url ~resolver doi in
  let request = Http.Request.make ~headers ~url `GET in
  let* response = Http_client.fetch httpc request in
  response_success request response

let bibtex = "application/x-bibtex; charset=utf-8"
let formatted_citation = "text/x-bibliography; charset=utf-8"
let json = "application/json; charset=utf-8"
