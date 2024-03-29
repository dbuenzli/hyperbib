(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* Label requests *)

module Url = struct
  type t =
  | Hyperbib_css
  | Hyperbib_js

  let dec u =
    let* `GET = Kurl.allow Http.Method.[get] u in
    match Kurl.Bare.path u with
    | ["hyperbib.css"] -> Kurl.ok Hyperbib_css
    | ["hyperbib.js"] -> Kurl.ok Hyperbib_js
    | _ -> Kurl.no_match

  let query = Http.Query.empty |> Http.Query.def "stamp" Stamp.static_files
  let enc = function
  | Hyperbib_css -> Kurl.Bare.v `GET ["hyperbib.css"] ~query
  | Hyperbib_js -> Kurl.Bare.v `GET ["hyperbib.js"] ~query

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
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
