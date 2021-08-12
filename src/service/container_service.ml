(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* Data lookups *)

let get_container = Entity_service.get_entity (module Container)
let get_container_ref_count db s =
  let ref_count = Reference.container_ref_count_stmt (Container.id s) in
  let* ref_count = Db.first' db ref_count in
  Ok (Option.value ~default:0 ref_count)

let get_page_data db g s =
  let only_public = Ask.Bool.v (Page.Gen.only_public g) in
  let all = Reference.list ~only_public in
  let id = Container.id s in
  let refs = Reference.filter_container_id (Ask.Int.v id) all in
  let* refs = Reference.render_data ~only_public refs db |> Db.error_resp in
  Ok refs

let get_container_for_page_ref =
  let page_url n id = Container.Url.v (Page (n, id)) in
  let page_404 = Container_html.page_404 in
  let entity_find_id_stmt = Container.find_id_stmt in
  let entity_public = Container.public in
  let entity_res_name = Container.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let view_fields_resp app db req id =
  let* c = get_container db id in
  let g = Webapp.page_gen app in
  let self = Container.Url.page c in (* assume comes from that page *)
  Ok (Page.resp_part (Container_html.view_fields g c ~self))

(* Responses *)

let confirm_delete app id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* c = get_container db id in
  let* ref_count = get_container_ref_count db c in
  let g = Webapp.page_gen app in
  let confirm = Container_html.confirm_delete g c ~ref_count in
  Ok (Page.resp_part confirm)

let create =
  let entity_page_url id = Container.Url.v (Page (None, id)) in
  Entity_service.create (module Container) ~entity_page_url

let delete =
  Entity_service.delete (module Container) ~deleted_html:Container_html.deleted

let duplicate app req src =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Container.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Container.table q in
  let* dst = Db.insert' db (Container.create_cols ~ignore_id:true vs) in
  let* () = Db.exec' db (Container.Label.copy_applications_stmt ~src ~dst) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Container.Url.v (Page (None, dst))) in
  Ok (Resp.empty ~headers Http.ok_200)

let duplicate_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* c = get_container db id in
  let c = Container.duplicate_data c in
  let g = Webapp.page_gen app in
  let duplicate_form = Container_html.duplicate_form g c in
  Ok (Page.resp_part duplicate_form)

let edit_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* c = get_container db id in
  let edit_form = Container_html.edit_form (Webapp.page_gen app) c in
  Ok (Page.resp_part edit_form)

let index app =
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* cs = Db.list db (Container.list_stmt ~only_public) in
  let ref_count = Reference.container_public_ref_count_stmt in
  let* ref_count = Db.id_map db ref_count fst in
  let page = Container_html.index g cs ~ref_count in
  Ok (Page.resp page)

let new_form app req ~cancel =
  let* () = Entity_service.check_edit_authorized app in
  let g = Webapp.page_gen app in
  let page = Container_html.new_form g Container.new' ~cancel in
  Ok (Page.resp page)

let page app ref =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* c = get_container_for_page_ref db g ~only_public ref in
  let* refs = get_page_data db g c in
  let page = Container_html.page g c refs in
  Ok (Page.resp page)

let replace app req this =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let* by = Entity.Url.replace_by_of_query q in
  if this = by then view_fields_resp app db req this else
  let rep = Reference.replace_container_stmt ~this ~by in
  let* () = Db.exec' db rep in
  let* () = Db.exec' db (Container.delete this) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Container.Url.v (Page (None, by))) in
  Ok (Resp.empty ~headers Http.ok_200)

let replace_form app req this =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* c = get_container db this in
  let* ref_count = get_container_ref_count db c in
  let* containers = Db.list' db (Container.list_stmt ~only_public:false) in
  let g = Webapp.page_gen app in
  let replace = Container_html.replace_form g c ~ref_count ~containers in
  Ok (Page.resp_part replace)

let update app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Container.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Container.table q in
  let* () = Db.exec' db (Container.update id vs) in
  let* c = get_container db id in
  let g = Webapp.page_gen app in
  let uf = Page.Gen.url_fmt g in
  let* refs = get_page_data db g c in
  let self = Container.Url.page c in (* assume comes from that page *)
  let html = Container_html.view_full g c ~self refs in
  let title = Container_html.page_full_title g c in
  let headers = Hfrag.hc_page_location_update uf self ~title () in
  Ok (Page.resp_part ~headers html)

let view_fields app req id =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r app sess req = match (r : Container.Url.t) with
| Confirm_delete id -> confirm_delete app id
| Create -> create app req
| Delete id -> delete app id
| Duplicate id -> duplicate app req id
| Duplicate_form id -> duplicate_form app req id
| Edit_form id -> edit_form app req id
| Index -> index app
| New_form { cancel } -> new_form app req ~cancel
| Page ref -> page app ref
| Replace id -> replace app req id
| Replace_form id -> replace_form app req id
| Update id -> update app req id
| View_fields id  -> view_fields app req id

let v = Kurl.service Container.Url.kind resp

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
