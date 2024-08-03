(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std


val page_404 : Page.Gen.t -> self:Kurl.t -> Page.t

val email_field : string
val bot_honeypot_field : string

val confirm_delete :
  Page.Gen.t -> Suggestion.t -> El.html

val suggest_form :
  ?force_rescue:bool -> ?msg:El.html -> Page.Gen.t -> Suggestion.t -> El.html

val created : Page.Gen.t -> Suggestion.t -> Page.t

val view_fields :
  ?no_ui:bool -> Page.Gen.t -> self:Kurl.t -> Suggestion.t -> El.html

val integrate : Page.Gen.t -> Suggestion.t -> form:El.html -> Page.t

val need_a_doi_or_suggestion : El.html

val index : Page.Gen.t -> Suggestion.t list -> is_full:bool -> Page.t
