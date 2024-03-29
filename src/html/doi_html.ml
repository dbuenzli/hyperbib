(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
