(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** CSS classes used throughout. *)

open Hyperbib.Std

val authors : At.t
val container : At.t
val container_loc : At.t
val container_ids : At.t
val creatable : At.t
val description : At.t
val doi : At.t
val edit_pages : At.t
val edit_ui : At.t
val editing : At.t
val editors : At.t
val entity : At.t
val entity_kind : At.t
val entity_menu : At.t
val entity_ui : At.t
val error : At.t
val fake_hr_bottom : At.t
val field : At.t
val hyperbib : At.t
val index : At.t
val item_count : At.t
val list : At.t
val letter : At.t
val letter_index : At.t
val login : At.t
val logout : At.t
val minus : At.t
val more_details : At.t
val p404 : At.t
val person : At.t
val private' : At.t
val remove : At.t
val replace : At.t
val hui_remove : At.t
val removable : At.t
val reference_ids : At.t
val ref : At.t
val ref_item : At.t
val ref_list : At.t
val testing : At.t
val toc : At.t
val secondary : At.t
val subject : At.t
val uppercase : At.t
val user_ui : At.t
val user_view : At.t
val value : At.t
val year : At.t

(** {1:animate Animate} *)

val fade : At.t


(** {1:msg Messages} *)

val message : At.t
val warn : At.t
val error : At.t
val info : At.t
val good : At.t

(** {1:util Utility} *)

val hspace_0 : At.t
val hspace_0_125 : At.t
val hspace_0_25 : At.t
val hspace_0_5 : At.t
val hspace : At.t
val hspace_2 : At.t
val hspace_4 : At.t
val hspace_8 : At.t

val vspace_000 : At.t
val vspace_0125 : At.t
val vspace_025 : At.t
val vspace_050 : At.t
val vspace_075 : At.t
val vspace_100 : At.t
val vspace_125 : At.t
val vspace_150 : At.t
val vspace_175 : At.t
val vspace_200 : At.t
val vspace_400 : At.t
val vspace_800 : At.t

module Font : sig
  val xx_small : At.t
  val x_small : At.t
  val small : At.t
  val normal : At.t
  val large : At.t
  val x_large : At.t
  val xx_large : At.t

  val w100 : At.t
  val w200 : At.t
  val w300 : At.t
  val w400 : At.t
  val w500 : At.t
  val w600 : At.t
  val w700 : At.t
  val w800 : At.t
  val w900 : At.t

  val w_light : At.t
  val w_normal : At.t
  val w_bold : At.t
end

module Margin : sig
  val top_000 : At.t
  val top_0125 : At.t
  val top_025 : At.t
  val top_050 : At.t
  val top_075 : At.t
  val top_100 : At.t
  val top_125 : At.t
  val top_150 : At.t
end

module Gap : sig
  val v_025 : At.t
  val v_050 : At.t
  val x_025 : At.t
  val x_050 : At.t
  val y_025 : At.t
  val y_050 : At.t
end


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
