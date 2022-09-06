(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

val find_dupe_doi :
  ?no_suggestion_dupe_check:bool ->
  Page.Gen.t -> self:Kurl.t -> Db.t -> string ->
  (El.html option, Http.resp) result

val lookup_doi :
  Service_env.t -> Doi.t ->
  (Doi.t * (Import.Doi.ref option, string) result, Http.resp) result

val empty_reference_form :
  ?msg:El.html -> Page.Gen.t -> self:Kurl.t ->
  cancel:Entity.Url.cancel_url -> El.html

val fill_in_reference_form :
  ?no_suggestion_dupe_check:bool->
  Service_env.t -> Db.t -> self:Kurl.t -> cancel:Entity.Url.cancel_url ->
  Doi.t -> (string option * El.html, Hyperbib.Std.Http.resp) result

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers

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
