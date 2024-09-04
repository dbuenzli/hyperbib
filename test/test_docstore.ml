(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Hyperbib_std
open Result.Syntax
open Bytesrw_xxhash

let k key = Xxh3_128.of_hex key |> Result.get_ok

let data0 = "abcd"
let data0_key = k "8d6b60383dfa90c21be79eecd1b1353d"

let data1 = "1234"
let data1_key = k "9a4dea864648af82823c8c03e6dd2202"

let no_key = k "aabbcc864648af82823c8c03e6dd2202"

let test () =
  Test.test "store mechanics" @@ fun () ->
  Result.get_ok @@ Result.join @@
  (* Make a store in a temporary directory. *)
  Os.Dir.with_tmp @@ fun dir ->
  let* store = Docstore.of_dir dir in
  (* Check that it looks empty. *)
  let* exists0 = Docstore.mem data0_key store in
  let* exists1 = Docstore.mem data1_key store in
  let* exists2 = Docstore.mem no_key store in
  assert (not exists0);  assert (not exists1); assert (not exists2);
  (* Add something and check that it exists. *)
  let* key0 = Docstore.add (Bytes.Reader.of_string data0) store in
  Test.eq (module Xxh3_128) key0 data0_key ~__POS__;
  let* exists0 = Docstore.mem key0 store in
  assert exists0;
  (* Add something else and check that it exists. *)
  let* key1 = Docstore.add (Bytes.Reader.of_string data1) store in
  Test.eq (module Xxh3_128) key1 data1_key ~__POS__;
  let* path = Docstore.find key1 store in
  (match path with None -> assert false | Some _ -> ());
  (* Read back a stored document. *)
  let* () = Docstore.with_doc key1 store @@ function
  | None -> assert false
  | Some r ->
      let data1' = Bytes.Reader.to_string r in
      Test.string data1' data1 ~__POS__;
  in
  (* Delete a stored document. *)
  let* existed = Docstore.delete key1 store in
  let* exists = Docstore.mem key1 store in
  assert (existed); assert (not exists);
  let* () = Docstore.with_doc key1 store @@ function
  | None -> ()
  | Some _ -> Test.fail "Expected no doc for key %a" Xxh3_128.pp key1 ~__POS__
  in
  (* Add something a second time. *)
  let* key0 = Docstore.add (Bytes.Reader.of_string data0) store in
  Test.eq (module Xxh3_128) key0 data0_key ~__POS__;
  (* Fold over the store keys. *)
  let add key path acc = match key with
  | Some key -> Test.eq (module Xxh3_128) key data0_key
  | None -> Test.fail "%a: File should not exist" Fpath.pp path
  in
  let* () = Docstore.fold add store () in
  Ok ()

let main () =
  Test.main @@ fun () ->
  test ();
  ()

let () = if !Sys.interactive then () else exit (main ())
