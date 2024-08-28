(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Char = B0_std.Char
module String = B0_std.String
module Bytes = Bytesrw.Bytes
module Result = B0_std.Result
module Fmt = B0_std.Fmt
module Log = B0_std.Log
module List = B0_std.List
module Fpath = B0_std.Fpath
module Cmd = B0_std.Cmd
module Os = B0_std.Os

module Bag = Rel_query.Bag

module Url = Webs_url
module Media_type = Webs.Media_type
module Http = Webs.Http
module Http_client = Webs.Http_client
module Res = Webs_bazaar.Res
module Kurl = Webs_bazaar.Kurl
module At = Htmlit.At
module El = Htmlit.El

module Bazaar = struct
  let cp_dir_content
      ?dotfiles ?follow_symlinks ~recurse ~of_dir:src ~inside_dir:dst ()
    =
    let cp _ _ rel () =
      Log.if_error ~use:() @@
      let src = Fpath.(src // rel) and dst = Fpath.(dst // rel) in
      Os.File.copy ~force:true ~make_path:true src ~dst
    in
    Os.Dir.fold_files ?dotfiles ?follow_symlinks ~rel:true ~recurse cp src ()

  open Rel

  let col_values ?(ignore = []) rt r =
    let eq_col (Col.Def c0) (Col.Def c1) = Col.equal_name c0 c1 in
    let add_col_value r acc (Col.Def col as colv) =
      if List.exists (eq_col colv) ignore then acc else
      Col.Value (col, Col.proj col r) :: acc
    in
    Row.fold (add_col_value r) [] rt
end
