(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

type address = string

let send ~sender ~recipient ~subject ~body =
  let msg =
    Fmt.str "From:%s\nTo:%s\nSubject:%s\n\n%s" sender recipient subject body
  in
  let stdin = Os.Cmd.in_string msg in
  (* FIXME don't hardcode the tool. *)
  Os.Cmd.run ~stdin Cmd.(arg "/usr/sbin/sendmail" % "-t")
