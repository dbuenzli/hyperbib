(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

module Date_md_partial_rel = struct
  let t =
    let enc = Date.md_partial_to_string in
    let dec = Date.md_partial_of_string in
    let pp = Date.pp_md_partial in
    Type.coded @@
    Type.Coded.make ~name:"date-md-partial" Type.text ~enc ~dec ~pp

  let v = Rel_query.Coded.v t
  let equal = Rel_query.Coded.equal t
  let ( = ) = equal
end


module Doi_rel = struct
  let t =
    let enc = Doi.to_string and dec = Doi.unsafe_of_string and pp = Doi.pp in
    Type.coded @@ Type.Coded.make ~name:"doi" Type.text ~enc ~dec ~pp

  let v = Rel_query.Coded.v t
  let equal = Rel_query.Coded.equal t
  let ( = ) = equal
end
