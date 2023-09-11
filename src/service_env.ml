(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Service environment *)

open Hyperbib.Std
open Result.Syntax

type editable = [ `No | `With_login | `Unsafe ]
type t =
  { conf : Hyperbib.Conf.t;
    caps : User.Caps.t;
    db_pool : Db.pool;
    editable : editable;
    email_sender : Email.address;
    max_pending_suggestions : int;
    notification_email : Email.address;
    page_gen : Page.Gen.t;
    static_dir : Fpath.t;
    suggestion_notification : bool; }

(* Properties *)

let conf e = e.conf
let caps e = e.caps
let editable e = e.editable
let email_sender e = e.email_sender
let notification_email e = e.notification_email
let max_pending_suggestions e = e.max_pending_suggestions
let page_gen e = e.page_gen
let static_dir e = e.static_dir
let suggestion_notification e = e.suggestion_notification
let url_fmt e = Page.Gen.url_fmt e.page_gen

let v ~conf ~caps ~db_pool ~editable ~page_gen () =
  (* FIXME store these things in the db or in a json file. *)
  let max_pending_suggestions = 30 in
  let email_sender = "vps@erratique.ch" in
  let notification_email = "bib@philoclimate.ch" in
  let suggestion_notification = true in
  let static_dir = Hyperbib.Conf.static_dir conf in
  { conf; caps; db_pool; editable; email_sender; max_pending_suggestions;
    notification_email; page_gen; static_dir; suggestion_notification }

let adjust e caps page_gen = { e with caps; page_gen }

(* Convenience database brackets *)

let with_db e f =
  Db.http_resp_error @@ Result.join @@ Rel_pool.with' e.db_pool f

let with_db' e f =
  Result.join @@ Db.http_resp_error @@ Rel_pool.with' e.db_pool f

let with_db_transaction k e f =
  Db.http_resp_error @@ Result.join @@ Result.join @@
  Rel_pool.with' e.db_pool (fun db -> Db.with_transaction k db f)

let with_db_transaction' k e f =
  Result.join @@ Db.http_resp_error @@ Result.join @@
  Rel_pool.with' e.db_pool (fun db -> Db.with_transaction k db f)

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
