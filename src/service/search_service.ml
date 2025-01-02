(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let search env user request =
  let page = Search_html.index (Service_env.page_gen env) () in
  Ok (Page.response page)

let response r env user request = match (r : Search.Url.t) with
| Index -> search env user request

let v = Kurl.service Search.Url.kind response
