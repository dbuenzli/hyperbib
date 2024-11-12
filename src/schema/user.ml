(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

(* Passwords *)

type algo = [ `Pbkdf2_hmac_sha_256 ]
type password =
  { algo : algo;
    iterations : int;
    salt : string; (* bytes *)
    key : string; (* bytes *) }

let password algo iterations salt key = { algo; iterations; salt; key }

let make_password password =
  let key_length = 32 in
  let iterations = 600_000 in
  let salt = Webs_hash.Sha_256.random_salt ~length:16 () in
  let key =
    Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
  in
  { algo = `Pbkdf2_hmac_sha_256; iterations; salt; key; }

(* Users *)

type t = { name : string; password : password; }
let user name password = { name; password }
let name u = u.name

type s = t String.Map.t
let empty = String.Map.empty
let is_empty = String.Map.is_empty
let mem ~name us = String.Map.mem name us
let remove ~name us = String.Map.remove name us
let add ~name ~password us =
  let password = make_password password in
  let user = user name password in
  String.Map.add name user us

let check ~name ~password us = match String.Map.find_opt name us with
| None -> false
| Some u ->
    let key_length = String.length u.password.key in
    let iterations = u.password.iterations in
    let salt = u.password.salt in
    let key =
      Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
    in
    Webs_hash.Sha_256.equal_key key u.password.key

let fold f us acc = String.Map.fold (fun _ u -> f u) us acc

(* Serialising *)

let algo_jsont =
  let assoc = ["pbkdf2-hmac-sha-256", `Pbkdf2_hmac_sha_256 ] in
  Jsont.enum ~kind:"algo" ~doc:"Password hashing algorithm" assoc

let password_jsont =
  Jsont.Object.map ~kind:"password" password
  |> Jsont.Object.mem "algo" algo_jsont ~enc:(fun p -> p.algo)
  |> Jsont.Object.mem "iterations" Jsont.int ~enc:(fun p -> p.iterations)
  |> Jsont.Object.mem "salt" Jsont.binary_string ~enc:(fun p -> p.salt)
  |> Jsont.Object.mem "key" Jsont.binary_string ~enc:(fun p -> p.key)
  |> Jsont.Object.finish

let user_jsont =
  Jsont.Object.map ~kind:"user" user
  |> Jsont.Object.mem "username" Jsont.string ~enc:(fun u -> u.name)
  |> Jsont.Object.mem "password" password_jsont ~enc:(fun u -> u.password)
  |> Jsont.Object.finish

let jsont =
  let kind = "users" and key u = u.name in
  Jsont.array_as_string_map ~kind ~key user_jsont

(* Persist *)

let save file us =
  let* json = Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont us in
  Os.File.write ~force:true ~make_path:true file json

let load file =
  let* exists = Os.File.exists file in
  if not exists then Ok empty else
  let* json = Os.File.read file in
  Jsont_bytesrw.decode_string jsont json

(* User capabilities *)

module Caps = struct
  type t = { edit : bool; see_private_data : bool; }
  let make ~edit ~see_private_data = { edit; see_private_data }
  let none = { edit = false; see_private_data = false }
  let edit c = c.edit
  let see_private_data c = c.see_private_data
end

(* Urls *)

module Url = struct
  type goto = string option

  let goto_key = "goto"
  let username_key = "username"
  let password_key = "password"

  let goto_of_query query = Http.Query.find_first goto_key query
  let goto_to_query goto = match goto with
  | None -> None |
    Some goto -> Some (Http.Query.empty |> Http.Query.def goto_key goto)

  type t =
  | Login of { goto : goto }
  | Authenticate of { goto : goto }
  | Logout of { goto : goto }
  | View of { private' : bool }

  let dec u = match Kurl.Bare.path u with
  | ["login"] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let goto = goto_of_query (Kurl.Bare.query u) in
      (match meth with
      | `GET -> Kurl.ok (Login { goto })
      | `POST -> Kurl.ok (Authenticate { goto }))
  | ["logout"] ->
      let* `POST = Kurl.allow Http.Method.[post] u in
      let goto = goto_of_query (Kurl.Bare.query u) in
      Kurl.ok (Logout { goto })
  | ["view"; "private"; private'] ->
      let* `POST = Kurl.allow Http.Method.[post] u in
      let* private' = match private' with
      | "true" -> Ok true
      | "false" -> Ok false
      | _ -> Http.Response.not_found_404 ()
      in
      Kurl.ok (View { private' })
  | _ ->
      Kurl.no_match

  let enc = function
  | Login { goto } ->
      Kurl.bare `GET ["login"] ?query:(goto_to_query goto)
  | Authenticate { goto } ->
      Kurl.bare `POST ["login"] ?query:(goto_to_query goto)
  | Logout { goto } ->
      Kurl.bare `POST ["logout"] ?query:(goto_to_query goto)
  | View { private' } ->
      let private' = if private' then "true" else "false" in
      Kurl.bare `POST ["view"; "private"; private']

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
end
