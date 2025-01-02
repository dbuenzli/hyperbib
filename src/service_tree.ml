(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
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
  |> Kurl.bind ["suggestions"] (immutable_session Suggestion_service.v)
  |> Kurl.bind ["years"] (immutable_session Year_service.v)
  |> Kurl.bind ["search"] (immutable_session Search_service.v)
  |> Kurl.bind ["users"] User_service.v

let url_fmt ~init =
  init
  |> Kurl.Fmt.bind_tree v
  |> Kurl.Fmt.bind [""] Static_file.Url.kind
