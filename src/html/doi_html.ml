(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let warn_doi_exists g ~self doi r =
  let url = Reference.Url.page r in
  let href = Kurl.Fmt.rel_url (Page.Gen.url_fmt g) ~src:self ~dst:url in
  let here = Hfrag.link ~href (El.txt_of String.lowercase_ascii Uimsg.here) in
  let at = [Hclass.message; Hclass.warn] in
  El.p ~at [El.txt_of Uimsg.document_in_bib doi; El.sp; here; El.txt "."]

let warn_doi_suggestion_exists g ~self doi s =
  let url = Suggestion.Url.v Index in
  let href = Kurl.Fmt.rel_url (Page.Gen.url_fmt g) ~src:self ~dst:url in
  let href = String.concat "#" [href; string_of_int (Suggestion.id s)] in
  let here = Hfrag.link ~href (El.txt_of String.lowercase_ascii Uimsg.here) in
  let at = [Hclass.message; Hclass.warn] in
  El.p ~at [El.txt_of Uimsg.document_suggested doi; El.sp; here; El.txt "."]
