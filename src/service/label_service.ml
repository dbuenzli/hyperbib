(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let resp r app sess req = Http.Response.not_implemented_501 ()
let v = Kurl.service Label.Url.kind resp
