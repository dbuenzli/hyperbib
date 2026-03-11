(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sending emails. *)

type address = string
(** the type for email addresses. *)

val send :
  sender:address -> recipient:address ->
  subject:string -> body:string -> (unit, string) result
(** [send ~sender ~recipient ~subject ~body] *)
