(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* TODO eventually this should simply be markdown
   rendered from user data.  *)

let page_html g =
  let b = Page.Gen.bibliography g in
  let updated = Page.Gen.now g in
  let crossref_org_href = "https://crossref.org" in
  let tz_offset_s = 3600 in
  let (y, m, d), ((hh,mm, _), _) = Ptime.to_date_time ~tz_offset_s updated in
  let link_project =
    let href = Bibliography.project_href b in
    let t = Bibliography.project_title b in
    Hfrag.link ~href (El.txt t)
  in
  let suggest =
    let href = Kurl.Fmt.url (Page.Gen.url_fmt g) (Suggestion.Url.v Index) in
    Hfrag.link ~href (El.txt Uimsg.your_suggestions_for_addition)
  in
  El.section [
    El.header [
      El.h1 [El.txt (Bibliography.project_title b);];
      El.p [El.txt "An annotated bibliography";
            El.span ~at:[At.class' "updated"]
              [ El.txt " last updated ";
                El.txt (Fmt.str "%d-%02d-%02d" y m d); El.txt " at ";
                El.txt (Fmt.str "%02d:%02d" hh mm); El.sp;
                El.txt "UTC+1"]]
    ];
    El.p [
      El.txt
        "The aim of this annotated bibliography is to provide a critical survey of the main relevant philosophical and scientific references dealing with foundational, epistemic and methodological issues related to climate science, climate modelling and climate change."
    ];
    El.p [ El.txt
             "The focus is on a qualitative survey of these issues in a philosophy of science perspective rather than on an exhaustive review of the literature. In a second stage, focused critical notes will be progressively attached to prominent entries."
         ];
    El.p [
      El.txt "The bibliography is compiled by collaborators of the ";
      link_project;
      El.txt " project who curate additions and the subject \
              classification. ";
      suggest; El.txt " are welcome.";
    ];
    El.p [
      El.strong [El.txt "Warning. "];
      El.txt "The current bibliographic data \
              was automatically sourced from ";
      Hfrag.link ~href:crossref_org_href (El.txt "Crossref");
      El.txt " as provided by the ";
      Hfrag.link ~href:Doi.default_resolver (El.txt "DOI system");
      El.txt ". This means that some of the data is noisy. In
          particular authors may end up being duplicated at the moment.
          Work is in progress to cleanup the data.";
    ]]

let page g =
  let self = Kurl.V (Bibliography.Url.kind, Home) in
  Page.with_content g ~self ~title:"About" ~content:(page_html g)

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
