(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hui programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html
open Ask.Std

let strf = Format.asprintf

let th ?(name = fun s -> El.txt s) c = El.th [name (Col.name c)]
let td ?data c r =
  let data = match data with
  | None -> El.txt (strf "%a" (Col.value_pp c) r)
  | Some data -> data (Col.proj c r)
  in
  El.td [data]

let tr_header ?ignore t =
  let cols = Table.cols ?ignore t in
  El.tr (List.map (fun (Col.V c) -> th c) cols)

let tr_data ?ignore t =
  let cols = Table.cols ?ignore t in
  fun r -> El.tr (List.map (fun (Col.V c) -> td c r) cols)

let table ?ignore t rows =
  let tr_data = tr_data ?ignore t in
  let thead = El.thead [tr_header ?ignore t] in
  let tbody = El.tbody (List.map tr_data rows) in
  El.table [thead; tbody]

let text_field ?at ?autocomplete ~edit ~col ~label r =
  let key = Ask.Col.name col and value = Ask.Col.proj col r in
  Hui.text_field ?at ?autocomplete ~edit ~key ~label ~value ()

let checkbox ?at ?value ~edit ~col ~label r =
  let key = Ask.Col.name col and checked = Ask.Col.proj col r in
  Hui.checkbox ?at ~edit ~key ~label ~checked ()
