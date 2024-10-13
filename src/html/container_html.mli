(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

val confirm_delete : Page.Gen.t -> Container.t -> ref_count:int -> El.html
val deleted : Page.Gen.t -> Container.t -> El.html

val new_form :
  Page.Gen.t -> Container.t -> cancel:Entity.Url.cancel_url -> Page.t

val edit_form : Page.Gen.t -> Container.t -> El.html
val duplicate_form : Page.Gen.t -> Container.t -> El.html
val replace_form : Page.Gen.t -> Container.t -> ref_count:int -> El.html
val view_fields : Page.Gen.t -> self:Kurl.t -> Container.t -> El.html
val view_full :
  Page.Gen.t -> self:Kurl.t -> Container.t -> Reference.render_data -> El.html

val page_404 : Page.Gen.t -> self:Kurl.t -> Page.t
val page_full_title : Page.Gen.t -> Container.t -> string
val page : Page.Gen.t -> Container.t -> Reference.render_data -> Page.t

val index :
  Page.Gen.t -> Container.t list ->
  ref_count:(Container.Id.t * int) Container.Id.Map.t -> Page.t
