(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Webs

(* DOIs *)

type t = string

let normalize = String.lowercase_ascii

(* Scraping *)

let is_quote c = Char.equal c '\"' || Char.equal c '\''
let doi_stop_char c = Char.Ascii.is_white c || Char.Ascii.is_control c

let simple_delimitation s ~suffix_start:start =
  match String.find_index ~start doi_stop_char s with
  | None -> String.length s - 1
  | Some stop_char -> stop_char - 1

and balanced_delimitation s ~suffix_start =
  (* Reasonably assume that if DOIs have brackets they are
     balanced. If we hit an unbalanced one we stop. When the context
     looks markupish we also try to suppress a final single or double
     quote. This allows extraction from HTML, CommonMark and
     BibTeX (albeit in this case they are escaped).  *)
  let rec loop s ~max ~pc ~sc ~cc ~ac i =
    let prev = i - 1 and next = i + 1 in
    if i > max then max else match s.[i] with
    | '(' -> loop s ~max ~pc:(pc + 1) ~sc ~cc ~ac next
    | '[' -> loop s ~max ~pc ~sc:(sc + 1) ~cc ~ac next
    | '{' -> loop s ~max ~pc ~sc ~cc:(cc + 1) ~ac next
    | '<' ->
        if s.[i + 1] = '/' && pc = 0 && sc = 0 && cc = 0 && ac = 0
        then prev else loop s ~max ~pc ~sc ~cc ~ac:(ac + 1) next
    | ')' ->
        if pc = 0 && sc = 0 && cc = 0 && ac = 0 then prev else
        loop s ~max ~pc:(pc - 1) ~sc ~cc ~ac next
    | ']' ->
        if pc = 0 && sc = 0 && cc = 0 && ac = 0 then prev else
        loop s ~max ~pc ~sc:(sc - 1) ~cc ~ac next
    | '}' ->
        if pc = 0 && sc = 0 && cc = 0 && ac = 0 then prev else
        loop s ~max ~pc ~sc ~cc:(cc - 1) ~ac next
    | '>' ->
        if pc = 0 && sc = 0 && cc = 0 && ac = 0
        then (if is_quote s.[prev] then prev - 1 else prev)
        else loop s ~max ~pc ~sc ~cc ~ac:(ac - 1) next
    | c when doi_stop_char c -> if is_quote s.[prev] then prev - 1 else prev
    | c -> loop s ~max ~pc ~sc ~cc ~ac next
  in
  loop s ~max:(String.length s - 1) ~pc:0 ~sc:0 ~cc:0 ~ac:0 suffix_start

let rec find ?(already_delimited = false) ?start s =
  let sub = "10." (* Note ISO 26324:2022 allows more directory indicator,
                     but could not find a registry, not sure if that exists. *)
  in
  match String.find_sub ?start ~sub s with
  | None -> None
  | Some first ->
      let start = first + String.length sub in
      let prefix_char c = Char.equal '.' c || Char.Ascii.is_digit c in
      match String.find_index ~start (Fun.negate prefix_char) s with
      | None -> find ~start s
      | Some should_slash ->
          if s.[should_slash] <> '/' then find ~start s  else
          let suffix_start = should_slash + 1 in
          let last =
            if first = 0 || already_delimited
            then simple_delimitation s ~suffix_start
            else balanced_delimitation s ~suffix_start
          in
          if last - suffix_start + 1 <= 0 then find ~start:(last + 1) s else
          Some (first, last)

let extract_range ~first ~last s =
  Webs.Http.Pct.decode (normalize (String.subrange ~first ~last s))

let fold_text_scrape f s acc =
  let rec loop f acc start s = match find ~start s with
  | None -> acc
  | Some (first, last) ->
      let doi = extract_range ~first ~last s in
      loop f (f doi acc) (last + 1) s
  in
  loop f acc 0 s

let find ?already_delimited ?start s =
  match find ~already_delimited:true ?start s with
  | None -> None | Some (first, last) -> Some (extract_range ~first ~last s)

let extract ?start s = find ?start s

(* Presentations *)

let default_resolver = "https://doi.org"

let as_uri d = Fmt.str "doi:%s" (Webs.Http.Pct.encode `Uri d)
let as_url ?(resolver = default_resolver) d =
  Fmt.str "%s/%s" resolver (Webs.Http.Pct.encode `Uri d)

(* Predicates and comparisons *)

let equal = String.equal
let compare = String.compare

(* Sets and maps *)

module Set = Set.Make (struct type nonrec t = t let compare = compare end)
module Map = Map.Make (struct type nonrec t = t let compare = compare end)

(* Converting *)

let pp = Fmt.string

let unsafe_of_string = Fun.id
let to_string = Fun.id
let of_string s = match find ~already_delimited:true s with
| None -> Error (Fmt.str "%S: No DOI found" s)
| Some d -> Ok d

(* Resolving *)

let response_success request response = match Http.Response.status response with
| 200 ->
    let* body = Http.Body.to_string (Http.Response.body response) in
    Ok (Some body)
| 404 -> Ok None
| status ->
    let method' = Http.Method.encode (Http.Request.method' request) in
    let* url = Http.Request.to_url request in
    Error (Printf.sprintf "%s on %s: responded with %d" method' url status)

let doi_url' ?resolver = function (* This can go once DOI becomes abstract *)
| "" -> Error "DOI is empty"
| doi -> Ok (as_url ?resolver doi)

let resolve_to_url ?resolver http doi =
  let* url = doi_url' ?resolver doi in
  let* request = Http.Request.of_url `GET ~url in
  let* response = Http_client.request ~follow:false http request in
  Http.Headers.(find' location) (Http.Response.headers response)

let resolve_to_content_type ?resolver ~content_type:ctype httpc doi
  =
  let headers = Http.Headers.(empty |> def accept ctype) in
  let* url = doi_url' ?resolver doi in
  let* request = Http.Request.of_url ~headers `GET ~url in
  let* response = Http_client.request ~follow:true httpc request in
  response_success request response

let bibtex = "application/x-bibtex; charset=utf-8"
let formatted_citation = "text/x-bibliography; charset=utf-8"
let json = "application/json; charset=utf-8"
