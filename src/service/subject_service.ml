(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

(* Data lookups *)

let select_subjects db ~only_public sel =
  (* FIXME only_public, FIXME Ask escape % and _ in selector, order by *)
  if String.trim sel = "" then Ok [] else
  let* ss = Db.list db (Subject.select_stmt sel) in
  Ok (List.sort Subject.order_by_name ss)

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
  let only_public = Rel_query.Bool.v only_public in
  let* parent = get_subject_parent db s in
  let all = Reference.list ~only_public in
  let id = Subject.id s in
  let refs = Reference.Subject.filter_subject_id (Rel_query.Int.v id) all in
  let* refs =
    Reference.render_data ~only_public refs db |> Db.http_resp_error
  in
  Ok (parent, refs)

let get_subject_for_page_ref =
  let page_url n id = Subject.Url.v (Page (n, id)) in
  let page_404 = Subject_html.page_404 in
  let entity_find_id_stmt = Subject.find_id_stmt in
  let entity_public = Subject.public in
  let entity_res_name = Subject.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let view_fields_resp env db req id =
  let* s = get_subject db id in
  let* parent = get_subject_parent db s in
  let g = Service_env.page_gen env in
  let* self = Html_kit.url_of_req_referer req in
  Ok (Page.part_response (Subject_html.view_fields g s ~self ~parent))

(* Responses *)

let confirm_delete env id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_subject db id in
  let* ref_count = get_subject_ref_count db s in
  let g = Service_env.page_gen env in
  let confirm = Subject_html.confirm_delete g s ~ref_count in
  Ok (Page.part_response confirm)

let create =
  let entity_page_url id = Subject.Url.v (Page (None, id)) in
  Entity_service.create (module Subject) ~entity_page_url

let delete =
  Entity_service.delete (module Subject) ~deleted_html:Subject_html.deleted

let duplicate env req src =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = [Col.Def Subject.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Subject.table q in
  let* dst = Db.insert' db (Subject.create_cols ~ignore_id:true vs) in
  let* () = Db.exec' db (Reference.Subject.copy_applications_stmt ~src ~dst) in
  let* () = Db.exec' db (Subject.Label.copy_applications_stmt ~src ~dst) in
  let uf = Service_env.url_fmt env in
  let headers = Html_kit.htmlact_redirect uf (Subject.Url.v (Page (None, dst))) in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let duplicate_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_subject db id in
  let* ref_count = get_subject_ref_count db s in
  let* parents = get_parents db in
  let s = Subject.duplicate_data s in
  let g = Service_env.page_gen env in
  let duplicate_form = Subject_html.duplicate_form g s ~ref_count ~parents in
  Ok (Page.part_response duplicate_form)

let edit_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_subject db id in
  let* parents = get_parents db in
  let g = Service_env.page_gen env in
  let edit_form = Subject_html.edit_form g s ~parents in
  Ok (Page.part_response edit_form)

let index env =
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* ss = Db.list db (Subject.list_stmt ~only_public) in
  let ref_count = Reference.subject_public_ref_count_stmt in
  let* ref_count = Db.id_map db ref_count fst in
  let page = Subject_html.index g ss ~ref_count in
  Ok (Page.response page)

let new_form env req ~cancel =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let* parents = Db.list' db Subject.parents_stmt in
  let page = Subject_html.new_form g Subject.new' ~parents ~cancel in
  Ok (Page.response page)

let page env ref =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* s = get_subject_for_page_ref db g ~only_public ref in
  let* parent, refs = get_page_data db g s in
  let page = Subject_html.page g s ~parent refs in
  Ok (Page.response page)

let replace env req this =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let* by = Entity.Url.replace_by_of_query q in
  if this = by then view_fields_resp env db req this else
  let copy = Reference.Subject.copy_applications_stmt ~src:this ~dst:by in
  let* () = Db.exec' db copy in
  let* () = Db.exec' db (Subject.delete this) in
  let uf = Service_env.url_fmt env in
  let headers = Html_kit.htmlact_redirect uf (Subject.Url.v (Page (None, by))) in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let replace_form env req this =
  (* TODO what handle children *)
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_subject db this in
  let* ref_count = get_subject_ref_count db s in
  let replace =
    Subject_html.replace_form (Service_env.page_gen env) s ~ref_count
  in
  Ok (Page.part_response replace)

let input env ~for_list ~input_name id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let* s = get_subject db id in
  let finder = match for_list with
  | true -> Entity_html.subject_input_finder uf ~for_list ~input_name
  | false -> El.void
  in
  let s = Entity_html.subject_input uf ~for_list ~input_name s in
  Ok (Page.part_response (El.splice [s; finder]))

let input_finder env ~for_list ~input_name =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let finder = Entity_html.subject_input_finder uf ~for_list ~input_name in
  Ok (Page.part_response finder)

let input_finder_find env ~for_list ~input_name sel =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let only_public = Page.Gen.only_public g in
  let* parents = Db.id_map db Subject.parents_stmt Subject.id in
  let* ss = select_subjects db ~only_public sel in
  let finder =
    Entity_html.subject_input_finder_results uf
      ~for_list ~input_name ~parents ss
  in
  Ok (Page.part_response finder)

let update env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = [Col.Def Subject.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Subject.table q in
  let* () = Db.exec' db (Subject.update id vs) in
  let* s = get_subject db id in
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let* parent, refs = get_page_data db g s in
  let* self = Html_kit.url_of_req_referer req in
  let title = Subject_html.page_full_title g s in
  let html = Subject_html.view_full g s ~self ~parent refs in
  let headers = Html_kit.htmlact_page_location_update uf self ~title () in
  Ok (Page.part_response ~headers html)

let view_fields env req id =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  view_fields_resp env db req id

let resp r env sess req = match (r : Subject.Url.t) with
| Confirm_delete id -> confirm_delete env id
| Create -> create env req
| Delete id -> delete env id
| Duplicate id -> duplicate env req id
| Duplicate_form id -> duplicate_form env req id
| Edit_form id -> edit_form env req id
| Index -> index env
| New_form { cancel } -> new_form env req ~cancel
| Page ref -> page env ref
| Replace id -> replace env req id
| Replace_form id -> replace_form env req id
| Input (for_list, input_name, id) -> input env ~for_list ~input_name id
| Input_create (for_list, n, s) -> Http.Response.not_implemented_501 ()
| Input_finder (for_list, input_name) -> input_finder env ~for_list ~input_name
| Input_finder_find (for_list, input_name, sel) ->
    input_finder_find env ~for_list ~input_name sel
| Update id -> update env req id
| View_fields id  -> view_fields env req id

let v = Kurl.service Subject.Url.kind resp
