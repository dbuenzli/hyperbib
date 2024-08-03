(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Web service. *)

open Hyperbib.Std

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

  val state : t Webs_session.State.t
  (** [state] is session state storing an authenticated user. *)

  type handler = (t, Webs_session.client_stored_error) Webs_session.Handler.t

  val handler :
    service_path:Http.Path.t ->
    private_key:Webs_authenticatable.Private_key.t -> secure_cookie:bool ->
    handler
  (** [handler] is a session handler for authenticated users. *)
end

(** {1:private_key Service private key setup} *)

val setup_private_key : file:Hyperbib.Std.Fpath.t ->
  (Webs_authenticatable.Private_key.t, string) result

(** {1:sub Sub services} *)

type sub =
  Service_env.t -> Session.t option -> Http.Request.t ->
  (Session.t Webs_session.response, Session.t Webs_session.response) result
(** The type for sub services of the web application. *)

type sub_with_immutable_session =
  Service_env.t -> Session.t option -> Http.Request.t ->
  (Http.Response.t, Http.Response.t) result
(** The type for sub services which do not change the session state. *)

val sub_with_immutable_session : sub_with_immutable_session -> sub
(** [sub_with_immutable_session s] serves the immutable service [s] and keeps
    the session unchanged. *)

(** {1:services Services} *)

type t = Service_env.t -> Http.Request.t -> Http.Response.t
(** The type for the web service. *)

val v :
  service_path:Http.Path.t -> private_key:Webs_authenticatable.Private_key.t ->
  secure_cookie:bool -> sub Kurl.tree ->
  fallback:sub_with_immutable_session -> t
(** [v ~service_path ~private_key ~secure_cookie tree ~fallback] serves
    [tree] with a fallback to [fallback] if nothing matches. The root
    of the service is on [service_path], [private_key] is for session
    authentication and [secure_cookie] indicates whether secure cookies
    should be enabled. *)
