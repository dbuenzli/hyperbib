(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let immutable_session s = Kurl.map_service Service.sub_with_immutable_session s

let v =
  Kurl.empty ()
  |> Kurl.bind [""] (immutable_session Bibliography_service.v)
  |> Kurl.bind ["labels"] (immutable_session Label_service.v)
  |> Kurl.bind ["subjects"] (immutable_session Subject_service.v)
  |> Kurl.bind ["containers"] (immutable_session Container_service.v)
  |> Kurl.bind ["persons"] (immutable_session Person_service.v)
  |> Kurl.bind ["references"] (immutable_session Reference_service.v)
  |> Kurl.bind ["years"] (immutable_session Year_service.v)
  |> Kurl.bind ["users"] User_service.v

let url_fmt ~init =
  init
  |> Kurl.Fmt.bind_tree v
  |> Kurl.Fmt.bind [""] Static_file.Url.kind

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
