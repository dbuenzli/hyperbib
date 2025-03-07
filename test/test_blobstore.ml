(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Hyperbib_std
open Result.Syntax
open Bytesrw_xxhash

let k key = Blobstore.Key.of_text key |> Result.get_ok

let data0 = "abcd"
let data0_key = k "8d6b60383dfa90c21be79eecd1b1353d.xxh3-128"

let data1 = "1234"
let data1_key = k "9a4dea864648af82823c8c03e6dd2202.xxh3-128"

let no_key = k "aabbcc864648af82823c8c03e6dd2202.xxh3-128"

let test_mechanics =
  Test.test "store mechanics" @@ fun () ->
  Result.get_ok @@ Result.join @@
  (* Make a store in a temporary directory. *)
  Os.Dir.with_tmp @@ fun dir ->
  let* store = Blobstore.of_dir dir in
  (* Check that it looks empty. *)
  let* exists0 = Blobstore.mem data0_key store in
  let* exists1 = Blobstore.mem data1_key store in
  let* exists2 = Blobstore.mem no_key store in
  Test.holds (not exists0) ~__POS__;
  Test.holds (not exists1) ~__POS__;
  Test.holds (not exists2) ~__POS__;
  (* Add something and check that it exists. *)
  let* key0, status0 = Blobstore.add (Bytes.Reader.of_string data0) store in
  Test.eq (module Blobstore.Key) key0 data0_key ~__POS__;
  Test.holds (status0 = Created) ~__POS__;
  let* exists0 = Blobstore.mem key0 store in
  Test.holds exists0 ~__POS__;
  (* Add something else and check that it exists. *)
  let* key1, status1 = Blobstore.add (Bytes.Reader.of_string data1) store in
  Test.eq (module Blobstore.Key) key1 data1_key ~__POS__;
  Test.holds (status1 = Created) ~__POS__;
  let* path = Blobstore.find key1 store in
  (match path with None -> assert false | Some _ -> ());
  (* Read back a stored document. *)
  let* () = Blobstore.with_blob key1 store @@ function
  | None -> assert false
  | Some r ->
      let data1' = Bytes.Reader.to_string r in
      Test.string data1' data1 ~__POS__;
  in
  (* Delete a stored document. *)
  let* existed = Blobstore.delete key1 store in
  let* exists = Blobstore.mem key1 store in
  assert (existed); assert (not exists);
  let* () = Blobstore.with_blob key1 store @@ function
  | None -> ()
  | Some _ ->
      Test.fail "Expected no blob for key %a" Blobstore.Key.pp key1 ~__POS__
  in
  (* Add something a second time. *)
  let* key0, status0 = Blobstore.add (Bytes.Reader.of_string data0) store in
  Test.eq (module Blobstore.Key) key0 data0_key ~__POS__;
  Test.holds (status0 = Exists) ~__POS__;
  (* Fold over the store keys. *)
  let add key path acc = match key with
  | Some key -> Test.eq (module Blobstore.Key) key data0_key
  | None -> Test.fail "%a: File should not exist" Fpath.pp path
  in
  let* () = Blobstore.fold add store () in
  Ok ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
