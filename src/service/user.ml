(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open B00_serialk_json

(* Passwords *)

let random_salt ~len =
  let () = Random.self_init () in
  let b = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set_uint8 b i (Random.int 256) done;
  Bytes.unsafe_to_string b

type password =
  { algo : [ `Pbkdf2_hmac_sha_256 ];
    iterations : int;
    salt : string; (* bytes *)
    key : string; (* bytes *) }

let password algo iterations salt key = { algo; iterations; salt; key }

let make_password pass =
  let key_len = 32 in
  let iterations = 400_000 in
  let salt = random_salt ~len:8 in
  let key = Webs_kit.Sha_256.pbkdf2_hmac ~key_len ~iterations ~pass ~salt () in
  { algo = `Pbkdf2_hmac_sha_256; iterations; salt; key; }

(* Users *)

type t = { name : string; password : password; }
let user name password = { name; password }

type s = t String.Map.t
let empty = String.Map.empty
let is_empty = String.Map.is_empty
let mem ~name us = String.Map.mem name us

let add ~name ~password us =
  let password = make_password password in
  let user = user name password in
  String.Map.add name user us

let check ~name ~password:pass us = match String.Map.find_opt name us with
| None -> false
| Some u ->
    let key_len = String.length u.password.key in
    let iterations = u.password.iterations in
    let salt = u.password.salt in
    let k = Webs_kit.Sha_256.pbkdf2_hmac ~key_len ~iterations ~pass ~salt () in
    Webs_kit.Sha_256.equal_key k u.password.key

(* Serialising

   That's a bit too much code :-) *)

let json_hex ~kind = Jsonq.string_to ~kind String.Ascii.of_hex'
let json_algo =
  let kind = "password hashing algorithm" in
  let algo a = match String.Ascii.lowercase a with
  | "pbkdf2-hmac-sha-256" -> Ok `Pbkdf2_hmac_sha_256
  | _ -> Fmt.error "%S: unknown %s" a kind
  in
  Jsonq.string_to ~kind:"hashing algorithm" algo

let json_password =
  let algo = Jsonq.(mem "algo" json_algo) in
  let iterations = Jsonq.(mem "iterations" int) in
  let salt = Jsonq.(mem "salt" (json_hex ~kind:"salt")) in
  let key = Jsonq.(mem "key" (json_hex ~kind:"key")) in
  Jsonq.(succeed password $ algo $ iterations $ salt $ key)

let json_user =
  let name = Jsonq.(mem "username" string) in
  let password = Jsonq.(mem "password" json_password) in
  Jsonq.(succeed user $ name $ password)

let of_json =
  let add_user u acc = String.Map.add u.name u acc in
  Jsonq.fold_array add_user json_user String.Map.empty

let to_json us =
  let algo = function `Pbkdf2_hmac_sha_256 -> "pbkdf2-hmac-sha-256" in
  let password p =
    Jsong.obj
    |> Jsong.mem "algo" (Jsong.string (algo p.algo))
    |> Jsong.mem "iterations" (Jsong.int p.iterations)
    |> Jsong.mem "salt" (Jsong.string (String.Ascii.to_hex p.salt))
    |> Jsong.mem "key" (Jsong.string (String.Ascii.to_hex p.key))
    |> Jsong.obj_end
  in
  let user u =
    Jsong.obj
    |> Jsong.mem "username" (Jsong.string u.name)
    |> Jsong.mem "password" (password u.password)
    |> Jsong.obj_end
  in
  let add_user _ u acc = Jsong.el (user u) acc in
  String.Map.fold add_user us Jsong.array |> Jsong.array_end

(* Persist *)

let save file us =
  let force = true and make_path = false in
  Os.File.write ~force ~make_path file (Jsong.to_string (to_json us))

let load file =
  let* exists = Os.File.exists file in
  if not exists then Ok empty else
  let* contents = Os.File.read file in
  let* json = Json.of_string ~file:(Fpath.to_string file) contents in
  Jsonq.query of_json json

(* User capabilities *)

module Caps = struct
  type t = { edit : bool }
  let v ~edit = { edit }
  let none = { edit = false }
  let edit c = c.edit
end

(* Urls *)

module Url = struct
  type goto = string option

  let goto_key = "goto"
  let username_key = "username"
  let password_key = "password"

  let goto_of_query query = Http.Query.find goto_key query
  let goto_to_query goto = match goto with
  | None -> None | Some goto -> Some (Http.Query.(empty |> add goto_key goto))

  type t =
  | Login of { goto : goto }
  | Authenticate of { goto : goto }
  | Logout of { goto : goto }
  | View of { private' : bool }

  let dec u = match Kurl.Bare.path u with
  | ["login"] ->
      let* meth = Kurl.Allow.(meths [get; post] u) in
      let goto = goto_of_query (Kurl.Bare.query u) in
      (match meth with
      | `GET -> Kurl.ok (Login { goto })
      | `POST -> Kurl.ok (Authenticate { goto }))
  | ["logout"] ->
      let* `POST = Kurl.Allow.(meths [post] u) in
      let goto = goto_of_query (Kurl.Bare.query u) in
      Kurl.ok (Logout { goto })
  | ["view"; "private"; private'] ->
      let* `POST = Kurl.Allow.(meths [post] u) in
      let* private' = match private' with
      | "true" -> Ok true
      | "false" -> Ok false
      | _ -> Resp.not_found_404 ()
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
