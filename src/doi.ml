(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs

let ( let*) = Result.bind

(* DOIs *)

type url = string
type t = string
let pp = Format.pp_print_string
let normalize = String.lowercase_ascii
let extract s =
  let s = String.trim s in
  match B0_std.String.find_sub ~sub:"10." s with
  | None -> normalize s
  | Some first -> normalize (B0_std.String.subrange ~first s)

(* Resolving *)

let default_resolver = "https://doi.org"

let response_success request response = match Http.Response.status response with
| 200 ->
    let* body = Http.Body.to_string (Http.Response.body response) in
    Ok (Some body)
| 404 -> Ok None
| status ->
    let method' = Http.Method.encode (Http.Request.method' request) in
    let* url = Http.Request.to_url request in
    Error (Printf.sprintf "%s on %s: responded with %d" method' url status)

let doi_url ~resolver = function
| "" -> Error "DOI is empty"
| doi -> Ok (Printf.sprintf "%s/%s" resolver (Webs.Http.Pct.encode `Uri doi))

let resolve_to_url ?(resolver = default_resolver) http doi =
  let* url = doi_url ~resolver doi in
  let* request = Http.Request.of_url `GET ~url in
  let* response = Http_client.request ~follow:false http request in
  Http.Headers.(find' location) (Http.Response.headers response)

let resolve_to_content_type
    ?(resolver = default_resolver) ~content_type:ctype httpc doi
  =
  let headers = Http.Headers.(empty |> def accept ctype) in
  let* url = doi_url ~resolver doi in
  let* request = Http.Request.of_url ~headers `GET ~url in
  let* response = Http_client.request ~follow:true httpc request in
  response_success request response

let bibtex = "application/x-bibtex; charset=utf-8"
let formatted_citation = "text/x-bibliography; charset=utf-8"
let json = "application/json; charset=utf-8"
