(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std


val confirm_delete : Page.Gen.t -> Subject.t -> ref_count:int -> El.html

val deleted : Page.Gen.t -> Subject.t -> El.html

val edit_form :
  Page.Gen.t -> Subject.t -> parents:Subject.t list -> El.html

val duplicate_form :
  Page.Gen.t -> Subject.t -> ref_count:int -> parents:Subject.t list -> El.html

val replace_form :
  Page.Gen.t -> Subject.t -> ref_count:int -> El.html

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
  Page.Gen.t -> Subject.t list ->
  ref_count:(Subject.Id.t * int) Subject.Id.Map.t -> Page.t
