(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

let date_md_partial_type =
  let enc = Date.md_partial_to_string in
  let dec = Date.md_partial_of_string in
  let pp = Date.pp_md_partial in
  Type.coded @@ Type.Coded.make ~name:"date-md-partial" Type.text ~enc ~dec ~pp

let doi_type =
  let enc = Doi.to_string in
  let dec = Doi.unsafe_of_string (* FIXME *) in
  Type.coded @@ Type.Coded.make ~name:"doi" Type.text ~enc ~dec ~pp:Doi.pp
