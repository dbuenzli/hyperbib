(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Webs_kit

(** Service environment.

    The service environment provides globals, current user
    capabilities and database access for services. *)

type editable = [ `No | `With_login | `Unsafe ]

type t
(** The type for service environments. Provides globals, current user
    capbilities and database access for services. *)

val v :
  conf:Hyperbib.Conf.t -> caps:User.Caps.t -> db_pool:Db.pool ->
  editable:editable -> page_gen:Page.Gen.t -> unit -> t
(** [v …] is an environment with given properties. See corresponding
    accessors for semantics. *)

val adjust : t -> User.Caps.t -> Page.Gen.t -> t

(** {1:props Properties} *)

val conf : t -> Hyperbib.Conf.t
(** [conf e] is the hyperbib configuration of [e]. *)

val caps : t -> User.Caps.t
(** [caps e] are the capabilities for the session. *)

val editable : t -> editable
(** [editable e] is the edition policy of [e]. *)

val email_sender : t -> Email.address
(** [email_sender e] is the email address used by the app to send emails. *)

val notification_email : t -> Email.address
(** [notification_email e] is the email address used for application
    notifications (e.g new suggestions). *)

val page_gen : t -> Page.Gen.t
(** [page_gen e] is the page generator for [e]. *)

val suggestion_notification : t -> bool
(** [suggestion_notification e] is [true] if new suggestion are notified
    to the {!notification_email}. *)

val static_dir : t -> Fpath.t
(** [static_dir e] is the directory in which static files can be found. *)

val url_fmt : t -> Kurl.fmt
(** [url_fmt e] is the URL formatter for [e]. *)

val max_pending_suggestions : t -> int
(** [max_pending_suggestions e] is the maximal number of
    pending suggestions allowed. *)

(** {1:brackets Convenience database brackets} *)

val with_db :
  t -> (Db.t -> ('a, Db.error) result) -> ('a, Http.resp) result

val with_db' :
  t -> (Db.t -> ('a, Http.resp) result) -> ('a, Http.resp) result

val with_db_transaction :
  Db.transaction_kind -> t -> (Db.t -> ('a, Db.error) result) ->
  ('a, Http.resp) result

val with_db_transaction' :
  Db.transaction_kind -> t -> (Db.t -> ('a, Http.resp) result) ->
  ('a, Http.resp) result

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
