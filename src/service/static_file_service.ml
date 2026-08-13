(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let v env _sess request =
  let config = Service_env.config env in
  let static_dir = Filepath.to_string (Hyperbib_config.static_dir config) in
  let* file = Http.Request.to_absolute_filepath ~file_root:static_dir request in
  let file = Filepath.v file in
  let file =
    if Filepath.is_syntactic_dir file
    then (* note because of path cleaning this is only for / *) file else
    if Filepath.exists_ext file then file else Filepath.(file + ".html")
  in
  let dir_response = Webs_fs.dir_index_file "index.html" |> Result.get_ok in
  let* response =
    Webs_fs.send_file ~dir_response request (Filepath.to_string file)
  in
  (* FIXME Webs_fs do something nice in send_file maybe *)
  match Filepath.take_ext ~multi:false file with
  | ".css" | ".js" | ".woff2" ->
      (* FIXME versioning scheme, note something was done in Static_file *)
      let forever = "public, max-age=31536000, immutable" in
      let hs = Http.Headers.(empty |> define cache_control forever) in
      Ok (Http.Response.override_headers ~by:hs response)
  | ".html" ->
      let ctrl = "max-age=0" in
      let hs = Http.Headers.(empty |> define cache_control ctrl) in
      Ok (Http.Response.override_headers ~by:hs response)
  | _ -> Ok response
