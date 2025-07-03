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
  match String.find_first_index ~start doi_stop_char s with
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
  match String.find_first ?start ~sub s with
  | None -> None
  | Some first ->
      let start = first + String.length sub in
      let prefix_char c = Char.equal '.' c || Char.Ascii.is_digit c in
      match String.find_first_index ~start (Fun.negate prefix_char) s with
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
  Webs.Url.Percent.decode (normalize (String.subrange ~first ~last s))

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

let as_uri d = Fmt.str "doi:%s" (Webs.Url.Percent.encode `Uri d)
let as_url ?(resolver = default_resolver) d =
  Fmt.str "%s/%s" resolver (Webs.Url.Percent.encode `Uri d)

let as_filename doi = String.map (function '/' | '\\' -> '_' | c -> c) doi

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

let v s = of_string s |> Result.get_ok'

let jsont =
  let kind = "DOI" in
  let dec = Jsont.Base.dec_result of_string in
  let enc = to_string in
  Jsont.Base.string (Jsont.Base.map ~kind ~dec ~enc ())

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

(* Resolving to document *)

let mozilla = "Mozilla/5.0"

let find_document_url_candidates ~doi ~media_type urls =
  (* Keep only URLs whose path have media_type's ext as a suffix and if there
     are multiple URLs with the same filename arbitrarily keep only one. *)
  let last_segment p = match String.rindex_opt p '/' with
  | None -> p | Some i -> String.subrange ~first:(i + 1) p
  in
  let ext = Media_type.to_file_ext media_type in
  let add (fnames, acc as ret) url = match Webs.Url.path url with
  | None -> ret
  | Some path ->
      if not (String.ends_with ~suffix:ext url) then ret else
      let fname = last_segment path in
      if String.Set.mem fname fnames then ret else
      let fnames = String.Set.add fname fnames in
      fnames, url :: acc
  in
  snd (List.fold_left add (String.Set.empty, []) urls)

let document_content_type_okay ~media_type ctype =
  ctype = media_type || ctype = "application/octet-stream"

let download_document_url httpc ~media_type url =
  let accept_types = media_type in
  let headers =
    Http.Headers.(empty |> def user_agent mozilla |> def accept accept_types)
  in
  let* request = Http.Request.of_url ~headers `GET ~url in
  let* response = Http_client.request httpc ~follow:true request in
  let url = (* FIXME add something to Http_client *)
    let headers = Http.Response.headers response in
    match Http.Headers.find Http_client.x_follow_location headers with
    | None -> url | Some url -> url
  in
  let status = Http.Response.status response in
  if status <> 200 then Fmt.error "%s: %a" url Http.Status.pp status else
  let body = Http.Response.body response in
  let ctype = Media_type.get_type (Http.Body.content_type body) in
  if document_content_type_okay ~media_type ctype then Ok (url, body) else
  Fmt.error "%s: Expected %s found: %s" url media_type ctype

let to_document ?(url_only = false) ?resolver httpc ~media_type doi =
  let accept_types =
    String.concat ", " [media_type; "text/html;q=0.5"; "text/plain;q=0.4"]
  in
  let headers =
    Http.Headers.(empty |> def user_agent mozilla |> def accept accept_types)
  in
  let url = as_url ?resolver doi in
  let* request = Http.Request.of_url ~headers `GET ~url in
  let* response = Http_client.request httpc ~follow:true request in
  let url = (* FIXME add something to Http_client *)
    let headers = Http.Response.headers response in
    match Http.Headers.find Http_client.x_follow_location headers with
    | None -> url | Some url -> url
  in
  let status = Http.Response.status response in
  if status <> 200 then Fmt.error "%s: %a" url Http.Status.pp status else
  let body = Http.Response.body response in
  let ctype = Media_type.get_type (Http.Body.content_type body) in
  if document_content_type_okay ~media_type ctype
  then Ok (url, if url_only then Http.Body.empty else body) else
  let* page = Http.Body.to_string body in
  let urls = Webs.Url.list_of_text_scrape ~root:url page in
  match find_document_url_candidates ~doi ~media_type urls with
  | [] -> Fmt.error "%s: No document URL found" url
  | [doc_url] ->
      if url_only then Ok (doc_url, Http.Body.empty) else
      download_document_url httpc ~media_type doc_url
  | urls ->
      Fmt.error "@[<v>%s: Giving up. Multiple URL document candidates@,%a@]"
        url Fmt.(list string) urls
