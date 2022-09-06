(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
