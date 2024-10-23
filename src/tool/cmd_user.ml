(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let add ~name ~password ~force conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  let users_file = Cli_kit.Conf.users_file conf in
  let* users = User.load users_file in
  match User.mem ~name users with
  | true when not force ->
      Log.err begin fun m ->
        m "User %a already exists. Use %a to bypass."
          Fmt.code name Fmt.code "--force"
      end;
      Ok Cli_kit.Exit.user_exists
  | _ ->
      let users = User.add ~name ~password users in
      let* () = User.save users_file users in
      Ok Cli_kit.Exit.ok

let list conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  let users_file = Cli_kit.Conf.users_file conf in
  let* users = User.load users_file in
  User.fold (fun u () -> Log.stdout (fun m -> m "%s" (User.name u))) users ();
  Ok Cli_kit.Exit.ok

let delete ~name conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  let users_file = Cli_kit.Conf.users_file conf in
  let* users = User.load users_file in
  match User.mem ~name users with
  | false ->
      Log.warn (fun m -> m "No user named %a." Fmt.code name);
      Ok Cli_kit.Exit.ok
  | true ->
      let users = User.remove ~name users in
      let* () = User.save users_file users in
      Ok Cli_kit.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

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
      `P "The $(tname) command adds an application user."; ]
  in
  Cli_kit.cmd_with_conf "add" ~doc ~man @@
  let+ name = username and+ password and+ force in
  add ~name ~password ~force

let delete_cmd =
  let doc = "Delete an applicatino user" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(tname) command deletes an application user."; ]
  in
  Cli_kit.cmd_with_conf "delete" ~doc ~man @@
  let+ name = username in
  delete ~name

let list_cmd =
  let doc = "Lists application users" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(tname) command lists application users."; ]
  in
  Cli_kit.cmd_with_conf "list" ~doc ~man @@
  Term.(const list)

let cmd =
  let doc = "Manage application users" in
  Cli_kit.cmd_group "user" ~doc @@
  [add_cmd; list_cmd]
