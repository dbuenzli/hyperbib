(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Typegist

(* Passwords *)

module Password = struct
  type algo = [ `Pbkdf2_hmac_sha_256 ]
  let algo_jsont = Jsont.enum ["pbkdf2-hmac-sha-256", `Pbkdf2_hmac_sha_256]
  let algo_gist =
    let pbkdf2_sha_256 =
      Type.Gist.case0 "`Pbkdf2_hmac_sha_256" `Pbkdf2_hmac_sha_256
    in
    let proj = function `Pbkdf2_hmac_sha_256 -> pbkdf2_sha_256 in
    let meta = Type.Gist.Meta.empty in
    let meta = Jsont_typegist.Meta.Jsont.add algo_jsont meta in
    Type.Gist.variant ~meta "algo" proj [pbkdf2_sha_256]

  type t =
    { algo : algo;
      iterations : int;
      salt : string; (* bytes *)
      key : string; (* bytes *) }

  let make' algo iterations salt key = { algo; iterations; salt; key }
  let make password =
    let key_length = 32 in
    let iterations = 600_000 in
    let salt = Webs_hash.Sha_256.random_salt ~length:16 () in
    let key =
      Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
    in
    { algo = `Pbkdf2_hmac_sha_256; iterations; salt; key; }

  let check ~password p =
    let key_length = String.length p.key in
    let iterations = p.iterations in
    let salt = p.salt in
    let key =
      Webs_hash.Sha_256.pbkdf2_hmac ~key_length ~iterations ~password ~salt ()
    in
    Webs_hash.Sha_256.equal_key key p.key

  let gist =
    Type.Gist.record "User.Password.t" make'
    |> Type.Gist.field "algo" algo_gist (fun p -> p.algo)
    |> Type.Gist.field "iterations" Type.Gist.int (fun p -> p.iterations)
    |> Type.Gist.field "salt" Type.Gist.string_as_bytes (fun p -> p.salt)
    |> Type.Gist.field "key" Type.Gist.string_as_bytes (fun p -> p.key)
    |> Type.Gist.finish_record
end

(* User *)

type t = { username : string; password : Password.t; }
let user username password = { username; password }
let name u = u.username
let password u = u.password
let gist =
  Type.Gist.record "User.t" user
  |> Type.Gist.field "username" Type.Gist.string_as_utf_8 name
  |> Type.Gist.field "password" Password.gist password
  |> Type.Gist.finish_record

let pp = Fun.Generic.pp gist

(* Users *)

type s = t String.Map.t
let empty = String.Map.empty
let is_empty = String.Map.is_empty
let mem ~name us = String.Map.mem name us
let remove ~name us = String.Map.remove name us
let add ~name ~password us =
  let password = Password.make password in
  let user = user name password in
  String.Map.add name user us

let check ~name ~password us = match String.Map.find_opt name us with
| None -> false (* XXX Maybe we should run a fake check here. *)
| Some u -> Password.check ~password u.password

let fold f us acc = String.Map.fold (fun _ u -> f u) us acc

let s_pp =
  let pp_user ppf (_, u) = pp ppf u in
  Fmt.vbox (Fmt.iter_bindings String.Map.iter pp_user)

let s_jsont =
  let kind = "User.s" and key u = u.username in
  Jsont.array_as_string_map ~kind ~key (Jsont_typegist.to_jsont gist)

(* Persist *)

let save file us =
  let* json = Jsont_bytesrw.encode_string ~format:Jsont.Indent s_jsont us in
  Os.File.write ~force:true ~make_path:true file json

let load file =
  let* exists = Os.File.exists file in
  if not exists then Ok empty else
  let* json = Os.File.read file in
  Jsont_bytesrw.decode_string s_jsont json

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
