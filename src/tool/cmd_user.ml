(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let add ~config ~username ~password ~force =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let users_file = Hyperbib_config.users_file config in
  let* users = User.load users_file in
  match User.mem ~name:username users with
  | true when not force ->
      Log.err begin fun m ->
        m "User %a already exists. Use %a to bypass."
          Fmt.code username Fmt.code "--force"
      end;
      Ok Hyperbib_cli.Exit.user_exists
  | _ ->
      let users = User.add ~name:username ~password users in
      let* () = User.save users_file users in
      Ok Hyperbib_cli.Exit.ok

let list ~config =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let users_file = Hyperbib_config.users_file config in
  let* users = User.load users_file in
  User.fold (fun u () -> Log.stdout (fun m -> m "%s" (User.name u))) users ();
  Ok Hyperbib_cli.Exit.ok

let delete ~config ~username =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let users_file = Hyperbib_config.users_file config in
  let* users = User.load users_file in
  match User.mem ~name:username users with
  | false ->
      Log.warn (fun m -> m "No user named %a." Fmt.code username);
      Ok Hyperbib_cli.Exit.ok
  | true ->
      let users = User.remove ~name:username users in
      let* () = User.save users_file users in
      Ok Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let exits = Hyperbib_cli.Exit.Info.user_cmd

let username =
  let doc = "The username." and docv = "USERNAME" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

let password =
  let doc = "The password." and docv = "PASSWORD" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let force =
  let doc = "Proceed even if user exists." in
  Arg.(value & flag & info ["f";"force"] ~doc)

let add_cmd =
  let doc = "Add an application user" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(cmd) command adds an application user."; ]
  in
  Cmd.make (Cmd.info "add" ~doc ~man ~exits) @@
  let+ config = Hyperbib_cli.config and+ username and+ password and+ force in
  add ~config ~username ~password ~force

let delete_cmd =
  let doc = "Delete an applicatino user" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(cmd) command deletes an application user."; ]
  in
  Cmd.make (Cmd.info "delete" ~doc ~man ~exits) @@
  let+ config = Hyperbib_cli.config and+ username in
  delete ~config ~username

let list_cmd =
  let doc = "Lists application users" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(cmd) command lists application users."; ]
  in
  Cmd.make (Cmd.info "list" ~doc ~man ~exits) @@
  let+ config = Hyperbib_cli.config in
  list ~config

let cmd =
  let doc = "Manage application users" in
  Cmd.group (Cmd.info "user" ~doc) @@
  [add_cmd; delete_cmd; list_cmd]
