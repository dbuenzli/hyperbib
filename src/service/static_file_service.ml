(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let v env _sess request =
  let static_dir = Fpath.to_string (Service_env.static_dir env) in
  let* file = Http.Request.to_absolute_filepath ~file_root:static_dir request in
  let file = Fpath.v file in
  let file = match Fpath.is_dir_path file with
  | true -> (* note because of path cleaning this is only for / *) file
  | false ->
      match Fpath.get_ext file with
      | "" -> Fpath.(file + ".html")
      | _ -> file
  in
  let dir_response = Webs_fs.dir_index_file "index.html" |> Result.get_ok in
  let* resp = Webs_fs.send_file ~dir_response request (Fpath.to_string file) in
  (* FIXME do something nice in send_file maybe *)
  let resp = match Fpath.get_ext file with
  | ".css" | ".js" | ".woff2" ->
      (* FIXME versioning scheme, note something was done in Static_file *)
      let forever = "public, max-age=31536000, immutable" in
      let hs = Http.Headers.(def cache_control) forever Http.Headers.empty in
      Http.Response.override_headers ~by:hs resp
  | ".html" ->
      let ctrl = "max-age=0" in
      let hs = Http.Headers.(def cache_control) ctrl Http.Headers.empty in
      Http.Response.override_headers ~by:hs resp
  | _ -> resp
  in
  Ok resp

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
