(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

type t =
  { copyrights : string;
    project_title : string;
    project_short_title : string;
    project_href : string;
    favicon_href : string option; }

let copyrights g = g.copyrights
let project_title g = g.project_title
let project_short_title g = g.project_short_title
let project_href g = g.project_href
let favicon_href g = g.favicon_href

let get () =
Ok { copyrights = "© University of Bern";
     project_title = "Epistemology of Climate Change";
     project_short_title = "philoclimate.ch";
     project_href = "https://philoclimate.ch";
     favicon_href = None; }

(* XXX think a bit abour Kurl and the bibtex file case. Do we always
   have enough context in dec ? Do we want to tuck a user defined param
   to be given to the service tree and/or find function ? *)

let bibtex_filename b =
  (* FIXME mangle. *)
  let mangle = function '.' | '/' | ':' | ';' | ' ' -> '_' | c -> c in
  String.map mangle (project_short_title b) ^ ".bib"

module Url = struct
  open Result.Syntax
  type bibliography = t
  type t =
  | Home
  | Help
  | Bibtex_file of string

  let dec_page u v = let* `GET = Kurl.Allow.(meths [get] u) in Kurl.ok v
  let dec u = match Kurl.Bare.path u with
  | [""] -> dec_page u Home
  | ["help"] -> dec_page u Help
  | [ bib ] when String.ends_with ~suffix:".bib" bib -> (* XXX ugly. *)
      dec_page u (Bibtex_file bib)
  | _ -> Kurl.no_match

  let enc_get_page p = Kurl.Bare.v `GET p ~ext:".html"
  let enc = function
  | Home -> enc_get_page [""]
  | Help -> enc_get_page ["help"]
  | Bibtex_file bib -> Kurl.Bare.v `GET [bib]

  let kind = Kurl.kind ~root_is:(`Dir (Some "index")) enc dec
  let v u = Kurl.v kind u
  let bibtex_file b = Kurl.v kind (Bibtex_file (bibtex_filename b))
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

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
