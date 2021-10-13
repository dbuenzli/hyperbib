(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Entity HTML commonalities. *)

open Hyperbib.Std



(** {1:editors Field viewers and editors} *)

val edit_description :
  (module Entity.DESCRIBABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_description :
  (module Entity.DESCRIBABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

(** {2:note Note field} *)

val edit_note :
  (module Entity.ANNOTABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_note :
  (module Entity.ANNOTABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

(** {2:public Public field} *)

val edit_public :
  (module Entity.PUBLICABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

val viz : (module Entity.PUBLICABLE with type t = 't) -> 't -> At.t
(** [viz (module E) e] is {!At.void} when [e] is public and
    {!Hclass.private'} otherwise. *)

(** {2:private_note Private note field} *)

val edit_private_note :
  (module Entity.PRIVATELY_ANNOTABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_private_note :
  (module Entity.PRIVATELY_ANNOTABLE with type t = 't) -> Page.Gen.t ->
  ?at:At.t list -> 't -> El.html

val removable :
  (module Entity.PUBLICABLE with type t = 't) ->
  ?suff:El.html -> key:string -> remove_tip:string ->
  render:('t -> El.html) -> 't -> El.html

(*
val addable :
  (module Entity.PUBLICABLE with type t = 't) ->
  key:string -> add_tip:string -> render:('t -> El.html) -> 't -> El.html

val addable_list :
  (module Entity.PUBLICABLE with type t = 't) ->
  key:string -> add_tip:string -> render:('t -> El.html) ->
  't list -> El.html
*)

val addable_subject_list :
  Kurl.fmt ->
  parents:Subject.t Id.Map.t -> Subject.t list -> El.html

val addable_contributor_list :
  Person.role option -> Kurl.fmt ->
  creatable:Person.t option -> Person.t list -> El.html

val add_subject : Kurl.fmt -> El.html
val add_contributor : Person.role option -> Kurl.fmt -> El.html
val removable_subject : Subject.t -> El.html
val removable_contributor : Person.role option -> Person.t -> El.html

val removable_contributor_create :
  Kurl.fmt -> Person.role option -> Person.t -> El.html

val addable_container_list :
  Kurl.fmt -> creatable:Container.t option -> Container.t list -> El.html
val add_container : Kurl.fmt -> El.html

val removable_container : Kurl.fmt -> Container.t -> El.html
val removable_container_create : Kurl.fmt -> Container.t -> El.html


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
