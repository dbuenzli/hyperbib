(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let results env user request query =
  Http.Response.not_implemented_501 ()

let search env user request query =
  let page = Search_html.index (Service_env.page_gen env) query in
  Ok (Page.response page)

let response r env user request = match (r : Search.Url.t) with
| Index query -> search env user request query
| Results query -> results env user request query

let v = Kurl.service Search.Url.kind response
