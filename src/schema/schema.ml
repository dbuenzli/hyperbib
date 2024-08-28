(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

type conf = string * string
let conf =
  let name = Col.make "name" Type.text fst in
  let value = Col.make "value" Type.text snd in
  let conf k v = k, v in
  Table.make "conf" @@
  Row.(unit conf * name * value)

let version = 1
let tables =
  Table.[ Def conf;
          Def Container.Label.table;
          Def Container.table;
          Def Doc.table;
          Def Label.table;
          Def Person.Label.table;
          Def Person.table;
          Def Reference.Cites.table;
          Def Reference.Contributor.table;
          Def Reference.Label.table;
          Def Reference.Subject.table;
          Def Reference.table;
          Def Subject.Label.table;
          Def Subject.See_also.table;
          Def Subject.table;
          Def Suggestion.table; ]


let v = Rel.Schema.make ~tables ()
