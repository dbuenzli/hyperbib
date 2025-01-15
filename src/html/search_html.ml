(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std


let search_results res =
  El.ol ~at:[Hclass.search_results] []

let search_bar g query =
  let query = Option.value ~default:"" None in
  let input =
    let at = [At.placeholder Uimsg.search; At.required; At.type' "search";
              At.autofocus; ]
    in
    Hui.input_string' ~min_size:25 ~at ~name:Search.Url.query_key query
  in
  let search =
    let tip = Uimsg.search_tip in
    let label = [Icon.magnifying_glass; El.span [El.txt Uimsg.search]] in
    Hui.button ~type':"submit" ~x_align:`Center ~dir:`H ~tip (El.splice label)
  in
  let at =
    let url = Search.Url.v (Results None) in
    let r = Html_kit.htmlact_request (Page.Gen.url_fmt g) url in
    let t = Htmlact.target ":up .search-results" in
    let layout = Negsp.Layout.sidebar ~gap:(`Sp `XXS) `End () in
    (Hclass.search :: r :: t :: layout)
  in
  El.splice [El.form ~at [input; search]; El.hr ()]

let index_html ?(results = search_results El.void) g ~self query =
  let h1 = El.h1 [El.txt Uimsg.search] in
  let at = Negsp.Text.flow () in
  El.section ~at [h1; search_bar g query; results]

let index ?(results = search_results El.void) g query =
  let self = Search.Url.v (Index query) in
  let content = index_html g ~self query ~results in
  Page.with_content g ~self ~title:Uimsg.search ~content
