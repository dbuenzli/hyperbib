(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let add conf name password force =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let users_file = Hyperbib.Conf.users_file conf in
  let* users = User.load users_file in
  match User.mem ~name users with
  | true when not force ->
      Log.err begin fun m ->
        m "User %s already exists. Use %a to bypass."
          name Fmt.(code string) "--force"
      end;
      Ok Hyperbib.Exit.user_exists
  | _ ->
      let users = User.add ~name ~password users in
      let* () = User.save users_file users in
      Ok Hyperbib.Exit.ok

let list conf =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let users_file = Hyperbib.Conf.users_file conf in
  let* users = User.load users_file in
  User.fold (fun u () -> Log.app (fun m -> m "%s" (User.name u))) users ();
  Ok Hyperbib.Exit.ok

(* Command line interface *)

open Cmdliner

let username =
  let doc = "The username." and docv = "USERNAME" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let pass =
  let doc = "The password." and docv = "PASSWORD" in
  Arg.(required & pos 2 (some string) None & info [] ~doc ~docv)

let force =
  let doc = "Proceed even if user exists." in
  Arg.(value & flag & info ["f";"force"] ~doc)

let add_cmd =
  let doc = "Add an application user" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man =
    [ `S Manpage.s_description;
      `P "The $(tname) command adds an application user."; ]
  in
  Cmd.v (Cmd.info "add" ~doc ~exits ~man)
    Term.(const add $ Hyperbib.Cli.conf $ username $ pass $ force)

let list_cmd =
  let doc = "Lists application users" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let man =
    [ `S Manpage.s_description;
      `P "The $(tname) command lists application users."; ]
  in
  Cmd.v (Cmd.info "list" ~doc ~exits ~man)
    Term.(const list $ Hyperbib.Cli.conf)

let cmd =
  let doc = "Manage application users" in
  let info = Cmd.info "user" ~doc in
  Cmd.group info [add_cmd; list_cmd]

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
