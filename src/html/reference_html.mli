(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
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
  Page.Gen.t -> self:Kurl.t -> cancel:Entity.Url.cancel_url ->
  Reference.t -> msg:El.html ->
  authors:Person.t Import.entity list -> editors:Person.t Import.entity list ->
  container:Container.t Import.entity option -> cites:Doi.t list -> El.html

val new_form :
  Page.Gen.t -> Reference.t -> cancel:Entity.Url.cancel_url -> Page.t

val deleted : Page.Gen.t -> Reference.t -> El.html

val undo_make_all_authors_public_button :
  Kurl.fmt -> Reference.id -> ids:Person.id list -> El.html

val view_authors :
  ?ui:El.html -> Kurl.fmt -> self:Kurl.t -> Person.t list -> El.html

val view_fields :
  ?authors_ui:El.html ->
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
