(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

module Url = struct
  open Result.Syntax

  type t = Index

  let dec u =
    let* `GET = Kurl.allow Http.Method.[get] u in
    match Kurl.Bare.path u with
    | [""] -> Kurl.ok Index
    | _ -> Kurl.no_match

  let enc = function
  | Index -> Kurl.Bare.v `GET [""]

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
end
