(*---------------------------------------------------------------------------
   Copyright (c) 2024 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type xxh3_128_hex = string
type id = xxh3_128_hex

type t =
  { id : id; reference : Reference.id option; slug : string;
    media_type : Media_type.t; }

let v ~id ~reference ~slug ~media_type =
  { id; reference; slug; media_type; }

let row id reference slug media_type =
  { id; reference; slug; media_type }

let id d = d.id
let reference d = d.reference
let slug d = d.slug
let media_type d = d.media_type

open Rel

let id' = Col.v "id" Type.Text id
let reference' = Col.v "reference" Type.(Option Int) reference
let slug' = Col.v "slug" Type.Text slug
let media_type' = Col.v "media_type" Type.Text media_type

let table =
  let primary_key = Col.[V id'] in
  let unique_keys = [Table.Unique_key.v Col.[V id'; V reference']] in
  let foreign_keys =
    [let cols = Col.[V reference'] in
     let parent =
       Table.Foreign_key.Parent (`Table Reference.table, [V Reference.id'])
     in
     let on_delete = `Set_null in
     Table.Foreign_key.v ~cols ~on_delete ~parent ()]
  in
  let indices =
    [Table.Index.v  Col.[V reference'];
     Table.Index.v  Col.[V id']]
  in
  Table.v "doc" ~primary_key ~unique_keys ~foreign_keys ~indices @@
  Row.(unit row * id' * reference' * slug' * media_type')
