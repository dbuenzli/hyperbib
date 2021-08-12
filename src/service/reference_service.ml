(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* Data lookups *)

let get_reference = Entity_service.get_entity (module Reference)

let get_reference_of_page_ref =
  let page_url n id = Reference.Url.v (Page (n, id)) in
  let page_404 = Reference_html.page_404 in
  let entity_find_id_stmt = Reference.find_id_stmt in
  let entity_public = Reference.public in
  let entity_res_name = Reference.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let get_reference_data db g r =
  Db.error_resp @@
  let only_public = Page.Gen.only_public g in
  let only_public = Ask.Bool.v only_public in
  let rid = Ask.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  Reference.render_data ~only_public ref db

let get_page_data db g r =
  (* The render_data calls could be streamlined to a single one. *)
  Db.error_resp @@
  let only_public = Page.Gen.only_public g in
  let only_public = Ask.Bool.v only_public in
  let rid = Ask.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  let* render_data = Reference.render_data ~only_public ref db in
  let cites = Reference.find_dois (Reference.dois_cited rid) in
  let* cites = Reference.render_data ~only_public cites db in
  let cited_by = match Reference.doi r with
  | None -> Bag.empty | Some doi -> Reference.citing_doi (Ask.Text.v doi)
  in
  let* cited_by = Reference.render_data ~only_public cited_by db in
  Ok (render_data, cites, cited_by)

let view_fields_resp app db req id =
  let* r = get_reference db id in
  let g = Webapp.page_gen app in
  let self = Reference.Url.page r in (* assume comes from that page *)
  let rid = Ask.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  let only_public = Page.Gen.only_public g in
  let only_public = Ask.Bool.v only_public in
  let* render_data =
    Reference.render_data ~only_public ref db |> Db.error_resp
  in
  Ok (Page.resp_part (Reference_html.view_fields g ~self r ~render_data))

(* Reponses *)

let confirm_delete app id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* r = get_reference db id in
  let g = Webapp.page_gen app in
  let confirm = Reference_html.confirm_delete g r in
  Ok (Page.resp_part confirm)

let create =
  let entity_page_url id = Reference.Url.v (Page (None, id)) in
  Entity_service.create (module Reference) ~entity_page_url

let delete =
  Entity_service.delete (module Reference) ~deleted_html:Reference_html.deleted

let edit_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* r = get_reference db id in
  let g = Webapp.page_gen app in
  let* render_data = get_reference_data db g r in
  let edit_form = Reference_html.edit_form g r ~render_data in
  Ok (Page.resp_part edit_form)

let index app =
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Ask.Bool.v (Page.Gen.only_public g) in
  let refs = Reference.list ~only_public in
  let* render_data = Reference.render_data ~only_public refs db in
  let page = Reference_html.index g render_data in
  Ok (Page.resp page)

let page app ref =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* r = get_reference_of_page_ref db g ~only_public ref in
  let* render_data, cites, cited_by = get_page_data db g r in
  let page = Reference_html.page g r ~render_data ~cites ~cited_by in
  Ok (Page.resp page)

let update app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Reference.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Reference.table q in
  let uniquify = true in
  let* sids =
    let key = Reference.Subject.(Hquery.key_for_rel table subject') in
    Hquery.find_ids ~uniquify key q
  in
  let* aids =
    let suff = Person.role_to_string Author in
    let key = Reference.Contributor.(Hquery.key_for_rel table person' ~suff) in
    Hquery.find_ids ~uniquify key q
  in
  let* eids =
    let suff = Person.role_to_string Editor in
    let key = Reference.Contributor.(Hquery.key_for_rel table person' ~suff) in
    Hquery.find_ids ~uniquify key q
  in
  let* () = Db.exec' db (Reference.update id vs) in
  let* () = Reference.Subject.set_list id sids db |> Db.error_resp in
  let* () =
    Reference.Contributor.set_list ~reference:id ~authors:aids ~editors:eids db
    |> Db.error_resp
  in
  let* r = get_reference db id in
  let g = Webapp.page_gen app in
  let* render_data, cites, cited_by = get_page_data db g r in
  let uf = Page.Gen.url_fmt g in
  let self = Reference.Url.page r in (* assume comes from that page *)
  let title = Reference_html.page_full_title g r in
  let html = Reference_html.view_full g ~self r ~render_data ~cites ~cited_by in
  let headers = Hfrag.hc_page_location_update uf self ~title () in
  Ok (Page.resp_part ~headers html)

let view_fields app req id =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r app sess req = match (r : Reference.Url.t) with
| Confirm_delete id -> confirm_delete app id
| Create -> create app req
| Delete id -> delete app id
| Edit_form id -> edit_form app req id
| Index -> index app
| New_form { cancel } -> Resp.server_error_500 () (* new_form app req ~cancel *)
| Page ref -> page app ref
| Update id -> update app req id
| View_fields id  -> view_fields app req id

let v = Kurl.service Reference.Url.kind resp

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
