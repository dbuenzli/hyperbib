(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

type config = string * string
let config =
  let name = Col.make "name" Type.text fst in
  let value = Col.make "value" Type.text snd in
  let config k v = k, v in
  Table.make "conf" @@
  Row.(unit config * name * value)

let version = 1
let tables =
  Table.[ Def config;
          Def Container.Label.table;
          Def Container.table;
          Def Label.table;
          Def Person.Label.table;
          Def Person.table;
          Def Reference.Cites.table;
          Def Reference.Contributor.table;
          Def Reference.Label.table;
          Def Reference.Subject.table;
          Def Reference.Doc.table;
          Def Reference.table;
          Def Subject.Label.table;
          Def Subject.See_also.table;
          Def Subject.table;
          Def Suggestion.table; ]

let v = Rel.Schema.make ~tables ()
