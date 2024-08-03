(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
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
