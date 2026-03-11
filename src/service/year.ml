(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

let filter ~year refs =
  let open Rel_query.Syntax in
  let* r = refs in
  let ref_year = r #. Reference.date_year' in
  Bag.where (Option.has_value ~eq:Int.( = ) year ref_year) (Bag.yield r)

let public_domain_stmt =
  let typ = Rel_sql.Stmt.(ret Row.(t2 (int "year") (int "count"))) in
  let sql =
    (* FIXME where is my nice query language ? *)
    Fmt.str "SELECT r.date_year, COUNT(*) FROM %s as r WHERE r.public \
             GROUP BY r.date_year" (Table.name Reference.table)
  in
  Rel_sql.Stmt.func sql typ

module Url = struct
  open Result.Syntax

  type t =
  | Index
  | Page of int

  let dec u =
    let* `GET = Kurl.allow Http.Method.[get] u in
    match Kurl.Bare.path u with
    | [""] -> Kurl.ok Index
    | [y] -> Result.map (fun y -> Some (Page y)) (Res.Id.decode y)
    | _ -> Kurl.no_match

  let html = ".html"
  let enc = function
  | Index -> Kurl.Bare.v `GET [""] ~ext:html
  | Page i -> Kurl.Bare.v `GET [Fmt.str "%d" i] ~ext:html

  let kind = Kurl.kind enc dec
  let v u = Kurl.v kind u
  let page y = Kurl.v kind (Page y)
end
