(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type raw_query = string

module Url = struct
  open Result.Syntax

  let query_key = "q"

  type t =
  | Index of raw_query option
  | Results of raw_query option

  let dec u =
    let* `GET = Kurl.allow Http.Method.[get] u in
    let query = Http.Query.find_first query_key (Kurl.Bare.query u) in
    match Kurl.Bare.path u with
    | [""] -> Kurl.ok (Index query)
    | ["part"; "results"] -> Kurl.ok (Results query)
    | _ -> Kurl.no_match


  let make_query q = match q with
  | None | Some "" -> Http.Query.empty
  | Some q -> Http.Query.add_value query_key q Http.Query.empty

  let enc = function
  | Index q -> Kurl.Bare.v `GET [""] ~query:(make_query q)
  | Results q ->
      (* TODO yHere again the enc/dec symmetry makes no sense. *)
      Kurl.Bare.v `GET ["part"; "results"] ~query:(make_query q)

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
end
