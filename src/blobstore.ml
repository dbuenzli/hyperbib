(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Bytesrw_xxhash

module Key = struct
  type t = Xxh3_128.t
  type text = string

  let ext = ".xxh3-128"
  let to_text h = Xxh3_128.to_hex h ^ ext
  let of_text t =
    if String.ends_with ~suffix:ext t
    then Xxh3_128.of_hex (String.sub t 0 (String.length t - String.length ext))
    else Fmt.error "Missing expected %s suffix" ext

  let equal = Xxh3_128.equal
  let compare = Xxh3_128.compare
  let pp ppf k = Fmt.string ppf (to_text k)
end

type t = { dir : Fpath.t }

let of_dir dir = Ok { dir }
let dir { dir } = dir

let key_of_filename fname = Result.to_option (Key.of_text fname)
let blobpath key store = Fpath.(store.dir / Key.to_text key)

let equal_files f0 f1 =
  Result.join @@
  Os.File.read_with_fd f0 @@ fun f0 ->
  Os.File.read_with_fd f1 @@ fun f1 ->
  let r0 = Bytesrw_unix.bytes_reader_of_fd f0 in
  let r1 = Bytesrw_unix.bytes_reader_of_fd f1 in
  Bytes.Reader.equal r0 r1

type add_status = Created | Exists | Collides

let add blob store =
  Result.join @@ Os.File.with_tmp_fd ~make_path:true ~dir:store.dir @@
  fun file fd ->
  try
    let w = Bytesrw_unix.bytes_writer_of_fd fd in
    let r, state = Xxh3_128.reads blob in
    let () = Bytes.Writer.write_reader ~eod:true w r in
    let key = Xxh3_128.value state in
    let blobpath = blobpath key store in
    let* exists = Os.File.exists blobpath in
    match exists with
    | true ->
        let* equal = equal_files file blobpath in
        if equal then Ok (key, Exists)  else Ok (key, Collides)
    | false ->
        let force = false and make_path = false in
        let* () = Os.Path.rename ~force ~make_path file ~dst:blobpath in
        Ok (key, Created)
  with
  | Unix.Unix_error (e, _, _) -> Fpath.error file "%s" (Unix.error_message e)
  | Bytes.Stream.Error e -> Fpath.error file "%s" (Bytes.Stream.error_message e)

let mem key store = Os.File.exists (blobpath key store)
let delete key store = Os.File.delete (blobpath key store)

let find key store =
  let blobpath = blobpath key store in
  let* exists = Os.File.exists blobpath in
  if exists then Ok (Some blobpath) else Ok None

let with_blob key store f =
  let blobpath = blobpath key store in
  let* exists = Os.File.exists blobpath in
  if not exists then Ok (f None) else
  Os.File.read_with_fd blobpath @@ fun fd ->
  f (Some (Bytesrw_unix.bytes_reader_of_fd fd))

let fold f store acc =
  let g st fname p acc = f (key_of_filename fname) p acc in
  let dotfiles = false and follow_symlinks = false and recurse = false in
  Os.Dir.fold_files ~dotfiles ~follow_symlinks ~recurse g store.dir acc
