(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let filter ~year refs =
  let open Rel_query.Syntax in
  let* r = refs in
  let ref_year = r #. Reference.date_year' in
  Bag.where (Option.has_value ~eq:Int.( = ) year ref_year) (Bag.yield r)

let public_domain_stmt =
  let typ = Sql.Stmt.(ret Row.(t2 (int "year") (int "count"))) in
  let sql =
    (* FIXME where is my nice query language ? *)
    Fmt.str "SELECT r.date_year, COUNT(*) FROM %s as r WHERE r.public \
             GROUP BY r.date_year" (Table.name Reference.table)
  in
  Sql.Stmt.func sql typ

module Url = struct
  open Result.Syntax

  type t =
  | Index
  | Page of int

  let dec u =
    let* `GET = Kurl.allow Http.Meth.[get] u in
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

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
