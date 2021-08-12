(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* Data lookups *)

let select_subjects db ~only_public sel =
  (* FIXME only_public, FIXME Ask escape % and _ in selector *)
  if String.trim sel = "" then Ok [] else
  Db.list db (Subject.select_stmt sel)

let get_subject = Entity_service.get_entity (module Subject)
let get_subject_ref_count db s =
  let ref_count = Reference.Subject.ref_count_stmt (Subject.id s) in
  let* ref_count = Db.first' db ref_count in
  Ok (Option.value ~default:0 ref_count)

let get_parents db = Db.list' db Subject.parents_stmt
let get_subject_parent db s = match Subject.parent s with
| None -> Ok None
| Some pid -> Db.first' db (Subject.find_id_stmt pid)

let get_page_data db g s =
  let only_public = Page.Gen.only_public g in
  let only_public = Ask.Bool.v only_public in
  let* parent = get_subject_parent db s in
  let all = Reference.list ~only_public in
  let id = Subject.id s in
  let refs = Reference.Subject.filter_subject_id (Ask.Int.v id) all in
  let* refs = Reference.render_data ~only_public refs db |> Db.error_resp in
  Ok (parent, refs)

let get_subject_for_page_ref =
  let page_url n id = Subject.Url.v (Page (n, id)) in
  let page_404 = Subject_html.page_404 in
  let entity_find_id_stmt = Subject.find_id_stmt in
  let entity_public = Subject.public in
  let entity_res_name = Subject.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let view_fields_resp app db req id =
  let* s = get_subject db id in
  let* parent = get_subject_parent db s in
  let g = Webapp.page_gen app in
  let self = Subject.Url.page s in (* assume comes from that page *)
  Ok (Page.resp_part (Subject_html.view_fields g s ~self ~parent))

(* Responses *)

let confirm_delete app id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* s = get_subject db id in
  let* ref_count = get_subject_ref_count db s in
  let g = Webapp.page_gen app in
  let confirm = Subject_html.confirm_delete g s ~ref_count in
  Ok (Page.resp_part confirm)

let create =
  let entity_page_url id = Subject.Url.v (Page (None, id)) in
  Entity_service.create (module Subject) ~entity_page_url

let delete =
  Entity_service.delete (module Subject) ~deleted_html:Subject_html.deleted

let duplicate app req src =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Subject.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Subject.table q in
  let* dst = Db.insert' db (Subject.create_cols ~ignore_id:true vs) in
  let* () = Db.exec' db (Reference.Subject.copy_applications_stmt ~src ~dst) in
  let* () = Db.exec' db (Subject.Label.copy_applications_stmt ~src ~dst) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Subject.Url.v (Page (None, dst))) in
  Ok (Resp.empty ~headers Http.ok_200)

let duplicate_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* s = get_subject db id in
  let* ref_count = get_subject_ref_count db s in
  let* parents = get_parents db in
  let s = Subject.duplicate_data s in
  let g = Webapp.page_gen app in
  let duplicate_form = Subject_html.duplicate_form g s ~ref_count ~parents in
  Ok (Page.resp_part duplicate_form)

let edit_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* s = get_subject db id in
  let* parents = get_parents db in
  let g = Webapp.page_gen app in
  let edit_form = Subject_html.edit_form g s ~parents in
  Ok (Page.resp_part edit_form)

let index app =
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* ss = Db.list db (Subject.list_stmt ~only_public) in
  let ref_count = Reference.subject_public_ref_count_stmt in
  let* ref_count = Db.id_map db ref_count fst in
  let page = Subject_html.index g ss ~ref_count in
  Ok (Page.resp page)

let new_form app req ~cancel =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let* parents = Db.list' db Subject.parents_stmt in
  let page = Subject_html.new_form g Subject.new' ~parents ~cancel in
  Ok (Page.resp page)

let page app ref =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* s = get_subject_for_page_ref db g ~only_public ref in
  let* parent, refs = get_page_data db g s in
  let page = Subject_html.page g s ~parent refs in
  Ok (Page.resp page)

let replace app req this =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let* by = Entity.Url.replace_by_of_query q in
  if this = by then view_fields_resp app db req this else
  let copy = Reference.Subject.copy_applications_stmt ~src:this ~dst:by in
  let* () = Db.exec' db copy in
  let* () = Db.exec' db (Subject.delete this) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Subject.Url.v (Page (None, by))) in
  Ok (Resp.empty ~headers Http.ok_200)

let replace_form app req this =
  (* TODO what handle children *)
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* s = get_subject db this in
  let* ref_count = get_subject_ref_count db s in
  let* subjects = Db.list' db (Subject.list_stmt ~only_public:false) in
  let g = Webapp.page_gen app in
  let replace = Subject_html.replace_form g s ~ref_count ~subjects in
  Ok (Page.resp_part replace)

let select app sel =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let uf = Page.Gen.url_fmt g in
  let only_public = Page.Gen.only_public g in
  let* parents = Db.id_map db Subject.parents_stmt Subject.id in
  let* ss = select_subjects db ~only_public sel in
  let sel = Entity_html.addable_subject_list uf ~parents ss in
  Ok (Page.resp_part sel)

let select_add app id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let* s = get_subject db id in
  let sel = Entity_html.add_subject (Page.Gen.url_fmt g) in
  let s = Entity_html.removable_subject s in
  let add = El.splice [s; sel] in
  Ok (Page.resp_part add)

let update app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Subject.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Subject.table q in
  let* () = Db.exec' db (Subject.update id vs) in
  let* s = get_subject db id in
  let g = Webapp.page_gen app in
  let uf = Page.Gen.url_fmt g in
  let* parent, refs = get_page_data db g s in
  let self = Subject.Url.page s in (* assume comes from that page *)
  let title = Subject_html.page_full_title g s in
  let html = Subject_html.view_full g s ~self ~parent refs in
  let headers = Hfrag.hc_page_location_update uf self ~title () in
  Ok (Page.resp_part ~headers html)

let view_fields app req id =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r app sess req = match (r : Subject.Url.t) with
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
| Select sel -> select app sel
| Select_add id -> select_add app id
| Update id -> update app req id
| View_fields id  -> view_fields app req id

let v = Kurl.service Subject.Url.kind resp

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
