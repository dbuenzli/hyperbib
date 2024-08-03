(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* TODO eventually this should simply be markdown
   rendered from user data.  *)

let page_html g =
  let doi_resolution_id = "doi-resolution" in
  El.section [
    El.h2 ~at:At.[id doi_resolution_id] [
      Hfrag.anchor_a doi_resolution_id;
      El.txt "Full text resolution"];
    El.p
      [El.txt
         "References are resolved to their full text by linking their DOI \
          to the root address ";
       Hfrag.link
         ~href:Doi.default_resolver (El.txt Doi.default_resolver);
       El.txt ". Another link root for the resolution can be specified \
               here:"];
    El.p [Page.doi_resolver];
    El.p
      [El.txt "This choice is persisted in your browser local storage."]
  ]

let page g =
  let self = Kurl.v Bibliography.Url.kind Help in
  Page.with_content g ~self ~title:"Help" ~content:(page_html g)
