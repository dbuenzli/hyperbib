(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Database entities service commonalities. *)

open Hyperbib.Std

(** {1:misc Miscellaneous} *)

val check_edit_authorized : Webapp.t -> (unit, Http.resp) result

(** {1:data Data lookups} *)

val get_entity :
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't) ->
  Db.t -> Id.t -> ('t, Http.resp) result

(** {1:responses Request responses} *)

val create :
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't) ->
  entity_page_url:(Id.t -> Kurl.t) ->
  Webapp.t -> Http.req -> (Http.resp, Http.resp) result

val delete :
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't) ->
  deleted_html:(Page.Gen.t -> 't -> El.html) ->
  Webapp.t -> Id.t -> (Http.resp, Http.resp) result

(** {1:pagerefs Page references} *)

type ('name, 'id) page_ref = 'name option * 'id

val entity_for_page_ref :
  page_url:('name option -> 'id -> Kurl.t) ->
  page_404:(Page.Gen.t -> self:Kurl.t -> Page.t) ->
  entity_find_id_stmt:('id -> 'entity Rel_sql.Stmt.t) ->
  entity_public:('entity -> bool) ->
  entity_res_name:('entity -> 'name) ->
  (Db.t -> Page.Gen.t -> only_public:bool ->
   ('name, 'id) page_ref -> ('entity, Http.resp) result)
(** XXX if we define a signature named resource we could use first class
    modules here, at least t replace the [entity_*] arguments
    For now it's a bit too disparate. *)

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
