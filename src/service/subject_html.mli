(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std


val confirm_delete : Page.Gen.t -> Subject.t -> ref_count:int -> El.html

val deleted : Page.Gen.t -> Subject.t -> El.html

val edit_form :
  Page.Gen.t -> Subject.t -> parents:Subject.t list -> El.html

val duplicate_form :
  Page.Gen.t -> Subject.t -> ref_count:int -> parents:Subject.t list -> El.html

val replace_form :
  Page.Gen.t -> Subject.t -> ref_count:int -> subjects:Subject.t list -> El.html

val new_form :
  Page.Gen.t -> Subject.t -> parents:Subject.t list ->
  cancel:Entity.Url.cancel_url -> Page.t

val view_fields :
  Page.Gen.t -> self:Kurl.t -> Subject.t -> parent:Subject.t option -> El.html

val view_full :
  Page.Gen.t -> self:Kurl.t -> Subject.t -> parent:Subject.t option ->
  Reference.render_data -> El.html

val page_404 : Page.Gen.t -> self:Kurl.t -> Page.t

val page_full_title : Page.Gen.t -> Subject.t -> string

val page :
  Page.Gen.t -> Subject.t -> parent:Subject.t option ->
  Reference.render_data -> Page.t

val index :
  Page.Gen.t -> Subject.t list -> ref_count:(Subject.id * int) Id.Map.t ->
  Page.t

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern.

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
