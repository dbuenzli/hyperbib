(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Reference HTML parts. *)

open Hyperbib.Std

(** {1:lists Lists} *)

val list_by_desc_date :
  Page.Gen.t -> self:Kurl.t -> Reference.render_data -> El.html

val list_section :
  ?anchor_id:string -> ?title:El.html -> Page.Gen.t -> self:Kurl.t ->
  descr:string -> descr_zero:string -> Reference.render_data -> El.html

(** {1:entity Entity} *)

val confirm_delete : Page.Gen.t -> Reference.t -> El.html

val edit_form :
  Page.Gen.t -> Reference.t -> render_data:Reference.render_data -> El.html

val filled_in_form :
  Page.Gen.t -> self:Kurl.t -> Reference.t -> msg:El.html ->
  authors:Person.t Import.entity list -> editors:Person.t Import.entity list ->
  container:Container.t Import.entity option -> cites:Doi.t list -> El.html

val new_form :
  Page.Gen.t -> Reference.t -> cancel:Entity.Url.cancel_url ->  Page.t

val deleted : Page.Gen.t -> Reference.t -> El.html

val view_fields :
  Page.Gen.t -> self:Kurl.t -> Reference.t ->
  render_data:Reference.render_data ->
  El.html

val view_full :
  Page.Gen.t -> self:Kurl.t -> Reference.t ->
  render_data:Reference.render_data ->
  cites:Reference.render_data -> cited_by:Reference.render_data ->
  El.html

val page_404 : Page.Gen.t -> self:Kurl.t -> Page.t
val page_full_title : Page.Gen.t -> Reference.t -> string
val page :
  Page.Gen.t -> Reference.t ->
  render_data:Reference.render_data ->
  cites:Reference.render_data ->
  cited_by:Reference.render_data -> Page.t

val index : Page.Gen.t -> Reference.render_data -> Page.t

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
