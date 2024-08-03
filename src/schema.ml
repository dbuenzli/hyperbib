(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Rel

type conf = string * string
let conf =
  let name = Col.v "name" Type.Text fst in
  let value = Col.v "value" Type.Text snd in
  let conf k v = k, v in
  let row = Row.(unit conf * name * value) in
  Table.v "conf" row

let version = 1
let tables =
  [ Table.V conf;
    Table.V Label.table;
    Table.V Person.table;
    Table.V Person.Label.table;
    Table.V Subject.table;
    Table.V Subject.Label.table;
    Table.V Subject.See_also.table;
    Table.V Container.table;
    Table.V Container.Label.table;
    Table.V Reference.table;
    Table.V Reference.Label.table;
    Table.V Reference.Contributor.table;
    Table.V Reference.Subject.table;
    Table.V Reference.Cites.table;
    Table.V Suggestion.table;
  ]

let v = Rel.Schema.v ~tables ()
