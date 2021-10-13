(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTML fragments used everywhere. *)

open Hyperbib.Std

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

(** {1:hc Hc requests} *)

val hc_request : Kurl.fmt -> Kurl.t -> At.t
(** [hc_request uf url] is an [hc-request] attribute for url [url]
    formatted by [uf]. *)

val hc_button :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> ?query:string -> target:string ->
  El.html -> El.html

val hc_delete :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> target:string -> El.html -> El.html

val hc_cancel :
  ?at:At.t list -> ?x_align:Hui.align -> ?align:Hui.align -> ?dir:Hui.dir ->
  ?tip:string -> Kurl.fmt -> Kurl.t -> target:string -> El.html -> El.html

(** {2:hc Hc entity buttons} *)

val target_entity : string
val target_entity_up : string

val hc_edit_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val hc_replace_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val hc_duplicate_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val hc_delete_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val hc_cancel_button : ?target:string -> Kurl.fmt -> Kurl.t -> El.html
val new_entity_button : href:string -> label:string -> El.html

(** {1:hc Hc responses} *)

val hc_page_location_update :
  ?init:Http.headers -> Kurl.fmt -> Kurl.t -> title:string -> unit ->
  Http.headers

val hc_redirect : ?init:Http.headers -> Kurl.fmt -> Kurl.t -> Http.headers

(** {1:links Links and anchors} *)

val anchor_href : string -> At.t
(** [anchor_href id] is an [href] attribute for anchor [id]. *)

val anchor_a : string -> El.html
(** [anchor_a id] is an anchor for [id]. *)

val link : ?at:At.t list -> href:string -> El.html -> El.html

(** {1:letters Letter indexes} *)

type letter = string

val h2_letter : letter -> Hyperbib.Std.El.html
val letters_nav : letter list -> Hyperbib.Std.El.html

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

(** {1:misc Miscellaneous} *)

val item_count : ?at:At.t list -> int -> El.html
(** [item_count n] is an {!Hclass.item_count} span for [n]. *)

val uppercase_span : ?at:At.t list -> string -> El.html
(** [uppercase_span] is an {!Hclass.uppercase} span. *)

val description : ?at:At.t list -> El.html -> El.html
(** [description p] is an {!Hclass.description} paragraph. *)

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
