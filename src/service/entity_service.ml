(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open Rel

let check_edit_authorized env =
  (* FIXME if the webapp is editable we should ask for login
     and retry the request, we might need a bit of `hc` support. *)
  match User.Caps.edit (Service_env.caps env) with
  | true -> Ok ()
  | false -> Http.Response.unauthorized_401 () (* FIXME do something for user *)


(* Data lookups *)

let get_entity
    (type t) (module E : Entity.IDENTIFIABLE_WITH_QUERIES with type t = t)
    db id
  =
  let* s = Db.first' db (E.find_id_stmt id) in
  match s with
  | None -> Http.Response.not_found_404 ()
  | Some s -> Ok s

(* Responses *)

let create
    (type t) (module E : Entity.IDENTIFIABLE_WITH_QUERIES with type t = t)
    ~entity_page_url env req
  =
  let* () = check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let* vs = Hquery.careless_find_table_cols ~ignore:[Col.V E.id'] E.table q in
  let* id = Db.insert' db (E.create_cols ~ignore_id:true vs) in
  let uf = Service_env.url_fmt env in
  let headers = Hfrag.hc_redirect uf (entity_page_url id) in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let delete
    (type t) (module E : Entity.IDENTIFIABLE_WITH_QUERIES with type t = t)
    ~deleted_html env id
  =
  let* () = check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* c = get_entity (module E) db id in
  let* () = Db.exec' db (E.delete id) in
  let deleted = deleted_html (Service_env.page_gen env) c in
  Ok (Page.part_response deleted)


(* Page references *)

type ('name, 'id) page_ref = 'name option * 'id

let entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name
  =
  fun db g ~only_public (req_name, req_id) ->
  let get_res id =
    let resp_page_404 ?explain req_name id =
      let page = page_404 g ~self:(page_url req_name id) in
      Error (Page.response_404 ?explain page)
    in
    let* p = Db.first' db (entity_find_id_stmt id) in
    match p with
    | None -> resp_page_404 req_name id
    | Some p when only_public && not (entity_public p) ->
        resp_page_404 ~explain:"not public" req_name id
    | Some p -> Ok p
  in
  let res_name = entity_res_name in
  let res_url n id = Kurl.Fmt.url (Page.Gen.url_fmt g) (page_url (Some n) id) in
  Res.Named.resolve ~get_res ~res_name ~res_url ~req_name ~req_id ()


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
