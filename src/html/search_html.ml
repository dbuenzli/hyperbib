(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let index_html g ~self () =
  let h1 = El.h1 [El.txt Uimsg.search; El.sp] in
  El.section [h1; El.p [El.txt "TODO"]]

let index g () =
  let self = Search.Url.v Index in
  let content = index_html g ~self () in
  Page.with_content g ~self ~title:Uimsg.search ~content
