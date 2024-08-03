(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

val confirm_delete : Page.Gen.t -> Person.t -> ref_count:int -> El.html
val deleted : Page.Gen.t -> Person.t -> El.html
val new_form : Page.Gen.t -> Person.t -> cancel:Entity.Url.cancel_url -> Page.t
val edit_form : Page.Gen.t -> Person.t -> El.html
val duplicate_form : Page.Gen.t -> Person.t -> ref_count:int -> El.html
val replace_form : Page.Gen.t -> Person.t -> ref_count:int -> El.html
val view_fields : Page.Gen.t -> self:Kurl.t -> Person.t -> El.html
val view_full :
  Page.Gen.t -> self:Kurl.t -> Person.t -> Reference.render_data -> El.html

val page_404 : Page.Gen.t -> self:Kurl.t -> Page.t
val page_full_title : Page.Gen.t -> Person.t -> string
val page : Page.Gen.t -> Person.t -> Reference.render_data -> Page.t
val index :
  Page.Gen.t -> Person.t list -> ref_count:(Person.id * int) Id.Map.t -> Page.t
