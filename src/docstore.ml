(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Bytesrw_xxhash

type key = Xxh3_128.t
type t = { dir : Fpath.t }

let of_dir dir = Ok { dir }
let dir { dir } = dir

let docpath key store =
  Fpath.(store.dir / Fmt.str "doc-%s" (Xxh3_128.to_hex key))

let key_of_filename fname = match String.cut_left ~sep:"-" fname with
| None -> None
| Some (pre, key) ->
    if pre <> "doc" then None else
    match Xxh3_128.of_hex key with
    | Error _ -> None | Ok key -> Some key

let equal_files f0 f1 =
  Result.join @@
  Os.File.read_with_fd f0 @@ fun f0 ->
  Os.File.read_with_fd f1 @@ fun f1 ->
  let r0 = Bytesrw_unix.bytes_reader_of_fd f0 in
  let r1 = Bytesrw_unix.bytes_reader_of_fd f1 in
  Bytes.Reader.equal r0 r1

let add doc store =
  Result.join @@ Os.File.with_tmp_fd ~make_path:true ~dir:store.dir @@
  fun file fd ->
  try
    let w = Bytesrw_unix.bytes_writer_of_fd fd in
    let r, state = Xxh3_128.reads doc in
    let () = Bytes.Writer.write_reader ~eod:true w r in
    let key = Xxh3_128.value state in
    let docpath = docpath key store in
    let* exists = Os.File.exists docpath in
    match exists with
    | true ->
        let* equal = equal_files file docpath in
        if equal then Ok key else
        Fmt.error "Not added, collision on hash %a" Xxh3_128.pp key
    | false ->
        let force = false and make_path = false in
        let* () = Os.Path.rename ~force ~make_path file ~dst:docpath in
        Ok key
  with
  | Unix.Unix_error (e, _, _) -> Fpath.error file "%s" (Unix.error_message e)
  | Bytes.Stream.Error e -> Fpath.error file "%s" (Bytes.Stream.error_message e)

let mem key store = Os.File.exists (docpath key store)
let delete key store = Os.File.delete (docpath key store)

let find key store =
  let docpath = docpath key store in
  let* exists = Os.File.exists docpath in
  if exists then Ok (Some docpath) else Ok None

let with_doc key store f =
  let docpath = docpath key store in
  let* exists = Os.File.exists docpath in
  if not exists then Ok (f None) else
  Os.File.read_with_fd docpath @@ fun fd ->
  f (Some (Bytesrw_unix.bytes_reader_of_fd fd))

let fold f store acc =
  let g st fname p acc = f (key_of_filename fname) p acc in
  Os.Dir.fold_files ~recurse:false g store.dir acc
