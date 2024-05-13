(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Cmdliner

let cmds =
  [ Cmd_user.cmd; Cmd_db.cmd; Cmd_html.cmd; Cmd_import.cmd;
    Cmd_serve.cmd; ]

let hyperbib =
  let doc = "Annotates bibliographies" in
  let exits = Hyperbib.Exit.Info.base_cmd in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  let info = Cmd.info "hyperbib" ~version:Stamp.version ~doc ~exits in
  Cmd.group info ~default cmds

let main () =
  Os.Exit.exit @@
  Log.time (fun _ m -> m "total time hyperbib %s" Stamp.version) @@ fun () ->
  match Cmd.eval_value' hyperbib with `Ok e -> e | `Exit c -> Os.Exit.code c

let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

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
