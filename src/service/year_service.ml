(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let list env =
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let* years = Db.list db Year.public_domain_stmt in
  let page = Year_html.index (Service_env.page_gen env) years in
  Ok (Page.response page)

let page env year =
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Rel_query.Bool.v (Page.Gen.only_public g) in
  let refs = Reference.list ~only_public in
  let refs = Year.filter ~year:(Rel_query.Int.v year) refs in
  let* render_data = Reference.render_data ~only_public refs db in
  let page = Year_html.page g ~year render_data in
  Ok (Page.response page)

let resp r env user req = match (r : Year.Url.t) with
| Index -> list env
| Page year -> page env year

let v = Kurl.service Year.Url.kind resp
