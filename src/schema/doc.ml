(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type xxh3_128_hex = string
type id = xxh3_128_hex

type t =
  { id : id; reference : Reference.id option; slug : string;
    media_type : Media_type.t; public : bool; origin : string; }

let v ~id ~reference ~slug ~media_type ~public ~origin =
  { id; reference; slug; media_type; public; origin; }

let row id reference slug media_type public origin =
  { id; reference; slug; media_type; public; origin; }

let id d = d.id
let reference d = d.reference
let slug d = d.slug
let media_type d = d.media_type
let public d = d.public
let origin d = d.origin

open Rel

let id' = Col.make "id" Type.text id
let reference' = Col.make "reference" Type.(option int) reference
let slug' = Col.make "slug" Type.text slug
let media_type' = Col.make "media_type" Type.text media_type
let public' = Col.make "public" Type.bool public
let origin' = Col.make "origin" Type.text origin

let table =
  let primary_key = Table.Primary_key.make [Def id'] in
  let unique_keys = [Table.Unique_key.make [Def id'; Def reference']] in
  let foreign_keys =
    [ Table.Foreign_key.make
        ~cols:[Def reference']
        ~parent:(Table (Reference.table, [Def Reference.id']))
        ~on_delete:`Set_null ()]
  in
  let indices =
    [ Table.Index.make [Def reference'];
      Table.Index.make [Def id']]
  in
  Table.make "doc" ~primary_key ~unique_keys ~foreign_keys ~indices @@
  Row.(unit row * id' * reference' * slug' * media_type' * public' * origin')
