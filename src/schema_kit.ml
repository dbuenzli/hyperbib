(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

let date_md_partial_type =
  let name = "date-md-partial" and pp = Date.pp_md_partial in
  let enc = Fun.compose Result.ok Date.md_partial_to_string in
  let dec = Date.md_partial_of_string in
  let coded = Type.Coded.v ~name ~pp enc dec Type.Text in
  Type.Coded coded
