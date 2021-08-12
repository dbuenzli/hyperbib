(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Web application setup and sessions. *)

open Hyperbib.Std
open Webs_kit

(** {1:session Sessions} *)

(** Authenticated user session state. *)
module Session : sig
  type t =
  | Unsafe of { private_view : bool }
  | User of { username : string; private_view : bool }
  (** The type for sessions. [Unsafe_edit] sessions. *)

  val private_view : t -> bool
  (** [private_view s] is [true] iff the private view is
      being show to [s]. *)

  val state : t Session.state
  (** [state] is session state storing an authenticated user. *)

  type handler = (t, Session.client_stored_error) Session.handler

  val handler :
    service_path:Http.path -> private_key:Authenticatable.private_key ->
    secure_cookie:bool -> handler
  (** [handler] is a session handler for authenticated users. *)
end

(** {1:setup Application setup} *)

type editable = [ `No | `With_login | `Unsafe ]

type t
(** The type for web application globals. *)

type service =
  t -> Session.t option -> Webs.Req.t -> Session.t Webs_kit.Session.result
(** The type for the services of the web application. *)

type immutable_session_service =
  t -> Session.t option -> Webs.Req.t -> (Webs.Resp.t, Webs.Resp.t) result
(** The type for services which do not change the session state. *)

val immutable_session_service : immutable_session_service -> service
(** [immutable_session_service s] serves with the immutable service
    [s] and keeps the session unchanged. *)

val for_serve : t ->
  User.Caps.t -> auth_ui:Page.Gen.auth_ui option ->
  user_view:Page.Gen.user_view option -> private_data:bool -> t
(** FIXME ugly. *)

val setup :
  backup_every_s:int option -> conf:Hyperbib.Conf.t ->
  data_conf:Hyperbib.Data_conf.t -> editable:editable -> max_connections:int ->
  secure_cookie:bool -> ?service_path:Http.path -> testing:bool ->
  unit -> (t, string) result
(** [setup ~conf ~data_conf ?service_path ~max_connections ~private_key
    ~backup_every_s ()] setups the web application with given attributes,
    see accessors for semantics.

    This makes sure the database exists and starts the stable backup
    thread (if any). *)

val serve : t ->
  url_fmt:(init:Kurl.fmt -> Kurl.fmt) ->
  (t -> Session.t option -> Webs.Req.t -> Session.t Webs_kit.Session.resp) ->
  Webs.service
(** [serve app s] serves service [s] with [app].

    {b FIXME.} This [url_fmt] makes things a bit messy. Maybe page_gen
    should not be part of `app` and we should thread something else in
    services. (e.g. a Responder.t value that packs everything,
    including one db connection lazily drawn from the pool. ?).  Maybe
    things will clariy when we get the bibliography conf out of the
    database. *)

val finish : t -> (unit, string) result
(** [finish app] terminates the application. This disposes the database
    connection pool. *)

(** {1:props Properties} *)

val backup_every_s : t -> int option
(** [backup_every_s a] is period (if any) at which stable backup of
    the database to {!Data_conf.db_file_backup} are peformed. *)

val conf : t -> Hyperbib.Conf.t
(** [conf a] is the hyperbib configuration of [a]. *)

val caps : t -> User.Caps.t
(** [caps a] are the capabilities for the session. *)

val data_conf : t -> Hyperbib.Data_conf.t
(** [data_conf a] is the hyperbib data configuration of [a]. *)

val editable : t -> editable
(** [editable a] is the edition policy of [a]. *)

val static_dir : t -> Fpath.t
(** [static_dir a] is the directory in which static files can be found. *)

val private_key : t -> Webs_kit.Authenticatable.private_key
(** [private_key a] is the private key used to authenticate user session
    data. *)

val page_gen : t -> Page.Gen.t
(** [page_gen a] is the page generator for [a]. *)

val secure_cookie : t -> bool
(** [secure_cookie a] is [true] if the session cookie is secure. *)

val url_fmt : t -> Kurl.fmt
(** [url_fmt a] is the URL formatter for [a]. *)

(** {1:bracket Convenience database brackets} *)

val with_db :
  t -> (Db.t -> ('a, Db.error) result) -> ('a, Resp.t) result

val with_db' :
  t -> (Db.t -> ('a, Resp.t) result) -> ('a, Resp.t) result

val with_db_transaction :
  Db.transaction_kind -> t -> (Db.t -> ('a, Db.error) result) ->
  ('a, Resp.t) result

val with_db_transaction' :
  Db.transaction_kind -> t -> (Db.t -> ('a, Resp.t) result) ->
  ('a, Resp.t) result

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
