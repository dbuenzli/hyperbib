(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Web service. *)

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

(** {1:private_key Service private key setup} *)

val setup_private_key : file:Hyperbib.Std.Fpath.t ->
  (Webs_kit.Authenticatable.private_key, string) result

(** {1:sub Sub services} *)

type sub =
  Service_env.t -> Session.t option -> Http.req ->
  (Session.t Webs_kit.Session.resp, Session.t Webs_kit.Session.resp) result
(** The type for sub services of the web application. *)

type sub_with_immutable_session =
  Service_env.t -> Session.t option -> Http.req -> (Http.resp, Http.resp) result
(** The type for sub services which do not change the session state. *)

val sub_with_immutable_session : sub_with_immutable_session -> sub
(** [sub_with_immutable_session s] serves the immutable service [s] and keeps
    the session unchanged. *)

(** {1:services Services} *)

type t = Service_env.t -> Http.req -> Http.resp
(** The type for the web service. *)

val v :
  service_path:Http.path -> private_key:Authenticatable.private_key ->
  secure_cookie:bool -> sub Kurl.tree ->
  fallback:sub_with_immutable_session -> t
(** [v ~service_path ~private_key ~secure_cookie tree ~fallback] serves
    [tree] with a fallback to [fallback] if nothing matches. The root
    of the service is on [service_path], [private_key] is for session
    authentication and [secure_cookie] indicates whether secure cookies
    should be enabled. *)

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
