(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTML generation commonalities. *)

open Hyperbib_std

(** {1:string String functions} *)

val sentencify : string -> string
(** [senticify s] adds a [.] at the end of [s] unless [s] is empty
      or ends with an US-ASCII punctation character. *)

val uncapitalize : string -> string
(** [uncapitalize s] is [s] uncapitalized. *)

val title : sub:string -> sup:string -> string
(** [title sub sup] is [String.concat " – " [sub; sup]]. *)

val ellipsify : string -> string
(** [ellipsify s] adds … at the end of [s]. *)

(** {1:htmlact_requests Htmlact requests} *)

val htmlact_request : Kurl.fmt -> Kurl.t -> At.t
(** [htmlact_request uf url] is an [htmlact-request] attribute for url [url]
    formatted by [uf]. *)

val htmlact_button :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> ?query:string -> ?target:string ->
  El.html -> El.html

val htmlact_delete :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> target:string -> El.html -> El.html

val htmlact_cancel :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> target:string -> El.html -> El.html

(** {2:htmlact_entity Htmlact entity buttons} *)

val target_entity : string
val target_entity_up : string

val htmlact_edit_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val htmlact_integrate_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val htmlact_replace_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val htmlact_duplicate_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val htmlact_delete_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val htmlact_cancel_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val new_entity_button : href:string -> label:string -> El.html

(** {1:htamlact_responses Htmlact responses} *)

val htmlact_page_location_update :
  ?init:Http.Headers.t -> Kurl.fmt -> Kurl.t -> title:string -> unit ->
  Http.Headers.t

val htmlact_redirect :
  ?init:Http.Headers.t -> Kurl.fmt -> Kurl.t -> Http.Headers.t

val url_of_req_referer : Http.Request.t -> (Kurl.t, Http.Response.t) result

(** {1:links Links and anchors} *)

val anchor_href : string -> At.t
(** [anchor_href id] is an [href] attribute for anchor [id]. *)

val anchor_a : string -> El.html
(** [anchor_a id] is an anchor for [id]. *)

val link : ?at:At.t list -> href:string -> El.html -> El.html

val doi_link : ?at:At.t list -> Doi.t option -> El.html -> El.html

val mailto_link :
  ?at:At.t list -> ?body:string ->
  ?subject:string -> email:string -> El.html -> El.html

(** {1:letters Letter indexes} *)

type letter = string

val h2_letter : letter -> Hyperbib_std.El.html
val letters_nav : letter list -> Hyperbib_std.El.html

(** {1:entity Entities}

    {b FIXME.} Maybe this should rather live in [Entity_html]. *)

val form_no_submit : ?at:At.t list -> El.html list -> El.html
val entity_form_no_submit : ?at:At.t list -> El.html list -> El.html

val entity_kind_index :
  Kurl.fmt -> self:Kurl.t -> kind:string -> Kurl.t -> El.html

(** {2:entity_links Links}

    HTML links to application entities pages. Gathering them here unties
    recursive dependencies in [*_html] modules. *)

type 'a linker = ?text:El.html -> Kurl.fmt -> self:Kurl.t -> 'a -> El.html
(** The type for functions generating links for entities of type ['a]. *)

val link_container : Container.t linker
val link_label : Label.t linker
val link_person : Person.t linker
val link_subject : Subject.t linker
val link_year : Date.year linker
val link_reference_doc :
  ?name:string -> Reference.t -> Reference.Doc.t linker

(** {1:misc Miscellaneous} *)

val item_count : ?at:At.t list -> int -> El.html
(** [item_count n] is an {!Hclass.item_count} span for [n]. *)

val uppercase_span : ?at:At.t list -> string -> El.html
(** [uppercase_span] is an {!Hclass.uppercase} span. *)

val description : ?at:At.t list -> El.html -> El.html
(** [description p] is an {!Hclass.description} paragraph. *)
