(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let fmt_year y = Fmt.str "%04d" y

let page_html g ~self ~year refs =
  let h1 = El.h1 [El.txt_of fmt_year year] in
  let descr = Uimsg.year_page_order_descr in
  let descr_zero = Uimsg.year_page_order_descr_zero in
  let refs = Reference_html.list_section g ~self ~descr ~descr_zero refs in
  El.section [h1; refs]

let page g ~year refs =
  let self = Year.Url.page year in
  let title = Hfrag.title ~sub:(fmt_year year) ~sup:Uimsg.year in
  let content = page_html g ~self ~year refs in
  Page.html g ~self ~title ~content

let year_index g ~self years =
  let year (y, c) =
    let text = El.splice [El.txt_of fmt_year y; El.sp; Hfrag.item_count c] in
    Hfrag.link_year (Page.Gen.url_fmt g) ~self ~text y
  in
  El.nav ~at:At.[Hclass.year; Hclass.index] (List.map year years)

let index_html g ~self years =
  let count = Hfrag.item_count (List.length years) in
  let h1 = El.h1 [El.txt Uimsg.years; El.sp; count] in
  let descr = El.p ~at:[Hclass.description] [El.txt Uimsg.year_index_descr] in
  let index = year_index g ~self years in
  El.section [h1; descr; index]

let index g years =
  let self = Year.Url.v Index in
  let content = index_html g ~self years in
  Page.html g ~self ~title:Uimsg.years ~content

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
