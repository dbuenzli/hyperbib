(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

(* Data lookups *)

let get_person = Entity_service.get_entity (module Person)
let get_person_ref_count db s =
  let ref_count = Reference.person_ref_count_stmt (Person.id s) in
  let* ref_count = Db.first' db ref_count in
  Ok (Option.value ~default:0 ref_count)

let get_page_data db g s =
  let only_public = Page.Gen.only_public g in
  let only_public = Ask.Bool.v only_public in
  let all = Reference.list ~only_public in
  let id = Person.id s in
  let refs = Reference.filter_person_id (Ask.Int.v id) all in
  let* refs = Reference.render_data ~only_public refs db |> Db.error_resp in
  Ok refs

let get_person_for_page_ref =
  let page_url n id = Person.Url.v (Page (n, id)) in
  let page_404 = Person_html.page_404 in
  let entity_find_id_stmt = Person.find_id_stmt in
  let entity_public = Person.public in
  let entity_res_name = Person.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let select_persons db ~only_public sel =
  (* FIXME only_public, FIXME Ask escape % and _ in selector *)
  if String.trim sel = "" then Ok [] else
  Db.list db (Person.select_stmt sel)


let view_fields_resp app db req id =
  let* p = get_person db id in
  let g = Webapp.page_gen app in
  let self = Person.Url.page p in (* assume comes from that page *)
  Ok (Page.resp_part (Person_html.view_fields g p ~self))

(* Responses *)

let confirm_delete app id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* p = get_person db id in
  let* ref_count = get_person_ref_count db p in
  let g = Webapp.page_gen app in
  let confirm = Person_html.confirm_delete g p ~ref_count in
  Ok (Page.resp_part confirm)

let create =
  let entity_page_url id = Person.Url.v (Page (None, id)) in
  Entity_service.create (module Person) ~entity_page_url

let delete =
  Entity_service.delete (module Person) ~deleted_html:Person_html.deleted

let duplicate app req src =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Person.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Person.table q in
  let* dst = Db.insert' db (Person.create_cols ~ignore_id:true vs) in
  let copy_contribs = Reference.Contributor.copy_contributions_stmt ~src ~dst in
  let* () = Db.exec' db copy_contribs in
  let* () = Db.exec' db (Person.Label.copy_applications_stmt ~src ~dst) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Person.Url.v (Page (None, dst))) in
  Ok (Resp.empty ~headers Http.ok_200)

let duplicate_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* p = get_person db id in
  let* ref_count = get_person_ref_count db p in
  let p = Person.duplicate_data p in
  let g = Webapp.page_gen app in
  let duplicate_form = Person_html.duplicate_form g p ~ref_count in
  Ok (Page.resp_part duplicate_form)

let edit_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* p = get_person db id in
  let edit_form = Person_html.edit_form (Webapp.page_gen app) p in
  Ok (Page.resp_part edit_form)

let index app =
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* ps = Db.list db (Person.list_stmt ~only_public) in
  let ref_count = Reference.persons_public_ref_count_stmt in
  let* ref_count = Db.id_map db ref_count fst in
  let page = Person_html.index g ps ~ref_count in
  Ok (Page.resp page)

let new_form app req ~cancel =
  let* () = Entity_service.check_edit_authorized app in
  let g = Webapp.page_gen app in
  let page = Person_html.new_form g Person.new' ~cancel in
  Ok (Page.resp page)

let page app ref =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* p = get_person_for_page_ref db g ~only_public ref in
  let* refs = get_page_data db g p in
  let page = Person_html.page g p refs in
  Ok (Page.resp page)

let replace app req this =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let* by = Entity.Url.replace_by_of_query q in
  if this = by then view_fields_resp app db req this else
  let copy = Reference.Contributor.copy_contributions_stmt ~src:this ~dst:by in
  let* () = Db.exec' db copy in
  let* () = Db.exec' db (Person.delete this) in
  let uf = Webapp.url_fmt app in
  let headers = Hfrag.hc_redirect uf (Person.Url.v (Page (None, by))) in
  Ok (Resp.empty ~headers Http.ok_200)

let replace_form app req this =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let* p = get_person db this in
  let* ref_count = get_person_ref_count db p in
  let* persons = Db.list' db (Person.list_stmt ~only_public:false) in
  let g = Webapp.page_gen app in
  let replace = Person_html.replace_form g p ~ref_count ~persons in
  Ok (Page.resp_part replace)

let select app role sel =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let uf = Page.Gen.url_fmt g in
  let only_public = Page.Gen.only_public g in
  let* ps = select_persons db ~only_public sel in
  let sel = Entity_html.addable_contributor_list role uf ps in
  Ok (Page.resp_part sel)

let select_add app role id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  let g = Webapp.page_gen app in
  let* p = get_person db id in
  let sel = Entity_html.add_contributor role (Page.Gen.url_fmt g) in
  let p = Entity_html.removable_contributor role p in
  let add = El.splice [p; sel] in
  Ok (Page.resp_part add)

let update app req id =
  let* () = Entity_service.check_edit_authorized app in
  Webapp.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Req.to_query req in
  let ignore = [Col.V Person.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Person.table q in
  let* () = Db.exec' db (Person.update id vs) in
  let* p = get_person db id in
  let g = Webapp.page_gen app in
  let uf = Page.Gen.url_fmt g in
  let* refs = get_page_data db g p in
  let self = Person.Url.page p in (* assume comes from that page *)
  let html = Person_html.view_full g p ~self refs in
  let title = Person_html.page_full_title g p in
  let headers = Hfrag.hc_page_location_update uf self ~title () in
  Ok (Page.resp_part ~headers html)

let view_fields app req id =
  Webapp.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r app sess req = match (r : Person.Url.t) with
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
| Select (role, sel) -> select app role sel
| Select_add (role, id) -> select_add app role id
| Update id -> update app req id
| View_fields id  -> view_fields app req id

let v = Kurl.service Person.Url.kind resp

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
