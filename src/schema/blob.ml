(*---------------------------------------------------------------------------
   Copyright (c) 2024 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

module Id = struct
  type t = Blobstore.Key.text
  module Rel = struct
    let type' = Rel_query.Sql.text
    let v = Rel_query.Text.v
    let equal = Rel_query.Text.equal
    let ( = ) = Rel_query.Text.( = )
  end
end

module Blob = struct
  type id = Id.t
  type t =
    { id : id;
      media_type : Media_type.t;
      origin : string;
      public : bool;
      slug : string; }

  let make ~id ~media_type ~origin ~public ~slug =
    { id; media_type; origin; public; slug }

  let row id media_type origin public slug =
    { id; media_type; origin; public; slug }

  let id b = b.id
  let media_type b = b.media_type
  let origin b = b.origin
  let public b = b.public
  let slug b = b.slug

  open Rel

  let id' = Col.make "id" Type.text id
  let media_type' = Col.make "media_type" Type.text media_type
  let origin' = Col.make "origin" Type.text origin
  let public' = Col.make "public" Type.bool public
  let slug' = Col.make "slug" Type.text slug

  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    Table.make "blob" ~primary_key @@
    Row.(unit row * id' * media_type' * origin' * public' * slug')
end

include Blob
include Entity.Publicable_queries (Id) (Blob)
