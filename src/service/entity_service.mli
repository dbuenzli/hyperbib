(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Database entities service commonalities. *)

open Hyperbib_std

(** {1:misc Miscellaneous} *)

val check_edit_authorized : Service_env.t -> (unit, Http.Response.t) result

(** {1:data Data lookups} *)

val get_entity :
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't and type Id.t = 'id)
  -> Db.t -> 'id -> ('t, Http.Response.t) result

(** {1:responses Request responses} *)

val create :
  (module Rel_kit.INTABLE_ID with type t = 'id) ->
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't and type Id.t = 'id)
  -> entity_page_url:('id -> Kurl.t) -> Service_env.t -> Http.Request.t ->
  (Http.Response.t, Http.Response.t) result

val delete :
  (module Entity.IDENTIFIABLE_WITH_QUERIES with type t = 't and type Id.t = 'id)
  -> deleted_html:(Page.Gen.t -> 't -> El.html) ->
  Service_env.t -> 'id -> (Http.Response.t, Http.Response.t) result

(** {1:pagerefs Page references} *)

type ('name, 'id) page_ref = 'name option * 'id

val entity_for_page_ref :
  page_url:('name option -> 'id -> Kurl.t) ->
  page_404:(Page.Gen.t -> self:Kurl.t -> Page.t) ->
  entity_find_id_stmt:('id -> 'entity Rel_sql.Stmt.t) ->
  entity_public:('entity -> bool) ->
  entity_res_name:('entity -> 'name) ->
  (Db.t -> Page.Gen.t -> only_public:bool ->
   ('name, 'id) page_ref -> ('entity, Http.Response.t) result)
(** XXX if we define a signature named resource we could use first class
    modules here, at least t replace the [entity_*] arguments
    For now it's a bit too disparate. *)
