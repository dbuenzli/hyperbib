(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open Rel

(* Data lookups *)

let get_person = Entity_service.get_entity (module Person)
let get_person_ref_count db s =
  let ref_count = Reference.person_ref_count_stmt (Person.id s) in
  let* ref_count = Db.first' db ref_count in
  Ok (Option.value ~default:0 ref_count)

let get_page_data db g s =
  let only_public = Page.Gen.only_public g in
  let only_public = Rel_query.Bool.v only_public in
  let all = Reference.list ~only_public in
  let id = Person.id s in
  let refs = Reference.filter_person_id (Rel_query.Int.v id) all in
  let* refs =
    Reference.render_data ~only_public refs db |> Db.http_resp_error
  in
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
  (* FIXME only_public, FIXME Rel escape % and _ in selector, order by *)
  if String.trim sel = "" then Ok [] else
  let* ps = Db.list db (Person.select_stmt sel) in
  Ok (List.sort Person.order_by_last_name ps)

let view_fields_resp app db req id =
  let* p = get_person db id in
  let g = Service_env.page_gen app in
  let* self = Hfrag.url_of_req_referer req in
  Ok (Page.resp_part (Person_html.view_fields g p ~self))

(* Responses *)

let confirm_delete env id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* p = get_person db id in
  let* ref_count = get_person_ref_count db p in
  let g = Service_env.page_gen env in
  let confirm = Person_html.confirm_delete g p ~ref_count in
  Ok (Page.resp_part confirm)

let create =
  let entity_page_url id = Person.Url.v (Page (None, id)) in
  Entity_service.create (module Person) ~entity_page_url

let delete =
  Entity_service.delete (module Person) ~deleted_html:Person_html.deleted

let duplicate env req src =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Req.to_query req in
  let ignore = [Col.V Person.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Person.table q in
  let* dst = Db.insert' db (Person.create_cols ~ignore_id:true vs) in
  let copy_contribs = Reference.Contributor.copy_contributions_stmt ~src ~dst in
  let* () = Db.exec' db copy_contribs in
  let* () = Db.exec' db (Person.Label.copy_applications_stmt ~src ~dst) in
  let uf = Service_env.url_fmt env in
  let headers = Hfrag.hc_redirect uf (Person.Url.v (Page (None, dst))) in
  Ok (Http.Resp.empty ~headers Http.ok_200)

let duplicate_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* p = get_person db id in
  let* ref_count = get_person_ref_count db p in
  let p = Person.duplicate_data p in
  let g = Service_env.page_gen env in
  let duplicate_form = Person_html.duplicate_form g p ~ref_count in
  Ok (Page.resp_part duplicate_form)

let edit_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* p = get_person db id in
  let edit_form = Person_html.edit_form (Service_env.page_gen env) p in
  Ok (Page.resp_part edit_form)

let index env =
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* ps = Db.list db (Person.list_stmt ~only_public) in
  let ref_count = Reference.persons_public_ref_count_stmt in
  let* ref_count = Db.id_map db ref_count fst in
  let page = Person_html.index g ps ~ref_count in
  Ok (Page.resp page)

let creatable_person_of_sel sel =
  let sel = String.trim sel in
  if sel = "" then None else
  match String.cut_left ~sep:"," sel with
  | None ->
      Option.some @@
      Person.v ~id:0 ~last_name:sel ~first_names:"" ~orcid:""
        ~note:"" ~private_note:"" ~public:true ()
  | Some (last_name, first_names) ->
      let last_name = String.trim last_name in
      let first_names = String.trim first_names in
      Option.some @@
      Person.v ~id:0 ~last_name ~first_names ~orcid:""
        ~note:"" ~private_note:"" ~public:true ()

let input env ~for_list ~input_name ~role id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let* p = get_person db id in
  let finder = match for_list with
  | true -> Entity_html.person_input_finder uf ~for_list ~input_name ~role
  | false -> El.void
  in
  let p = Entity_html.person_input uf ~for_list ~input_name ~role p in
  Ok (Page.resp_part (El.splice [p; finder]))

let input_create env ~for_list ~input_name ~role p =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let p = Entity_html.person_input_create uf ~for_list ~input_name ~role p in
  let finder = match for_list with
  | true -> Entity_html.person_input_finder uf ~for_list ~input_name ~role
  | false -> El.void
  in
  Ok (Page.resp_part (El.splice [p; finder]))

let input_finder env ~for_list ~input_name ~role =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let finder = Entity_html.person_input_finder uf ~for_list ~input_name ~role in
  Ok (Page.resp_part finder)

let input_finder_find env ~for_list ~input_name ~role sel =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let only_public = Page.Gen.only_public g in
  let* ps = select_persons db ~only_public sel in
  let creatable = creatable_person_of_sel sel in
  let res =
    Entity_html.person_input_finder_results
      uf ~for_list ~input_name ~role ~creatable ps
  in
  Ok (Page.resp_part res)

let new_form env req ~cancel =
  let* () = Entity_service.check_edit_authorized env in
  let g = Service_env.page_gen env in
  let page = Person_html.new_form g Person.new' ~cancel in
  Ok (Page.resp page)

let page env ref =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* p = get_person_for_page_ref db g ~only_public ref in
  let* refs = get_page_data db g p in
  let page = Person_html.page g p refs in
  Ok (Page.resp page)

let _replace_form env db this =
  let* p = get_person db this in
  let* ref_count = get_person_ref_count db p in
  let g = Service_env.page_gen env in
  let replace = Person_html.replace_form g p ~ref_count in
  Ok (Page.resp_part replace)

let replace env req this =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Req.to_query req in
  let* by =
    let* by = Entity.Url.replace_by_of_query' q in
    match by with
    | Some _ as by -> Ok by
    | None ->
        let* public =
          let* p = get_person db this in
          Ok (Person.public p)
        in
        match Hquery.find_create_person ~public ~role:None q with
        | None -> Ok None
        | Some p ->
            let* pid = Db.insert' db (Person.create ~ignore_id:true p) in
            Ok (Some pid)
  in
  match by with
  | None ->
      (* FIXME solve that front end
         the request should not be done if we don't have the data *)
      _replace_form env db this
  | Some by ->
      if this = by then view_fields_resp env db req this else
      let copy =
        Reference.Contributor.copy_contributions_stmt ~src:this ~dst:by
      in
      let* () = Db.exec' db copy in
      let* () = Db.exec' db (Person.delete this) in
      let uf = Service_env.url_fmt env in
      let headers = Hfrag.hc_redirect uf (Person.Url.v (Page (None, by))) in
      Ok (Http.Resp.empty ~headers Http.ok_200)

let replace_form env req this =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  _replace_form env db this

let update env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Req.to_query req in
  let ignore = [Col.V Person.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Person.table q in
  let* () = Db.exec' db (Person.update id vs) in
  let* p = get_person db id in
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let* refs = get_page_data db g p in
  let* self = Hfrag.url_of_req_referer req in
  let html = Person_html.view_full g p ~self refs in
  let title = Person_html.page_full_title g p in
  let headers = Hfrag.hc_page_location_update uf self ~title () in
  Ok (Page.resp_part ~headers html)

let view_fields env req id =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  view_fields_resp env db req id

let resp r env sess req = match (r : Person.Url.t) with
| Confirm_delete id -> confirm_delete env id
| Create -> create env req
| Delete id -> delete env id
| Duplicate id -> duplicate env req id
| Duplicate_form id -> duplicate_form env req id
| Edit_form id -> edit_form env req id
| Index -> index env
| Input (for_list, input_name, role, id) ->
    input env ~for_list ~input_name ~role id
| Input_create (for_list, input_name, role, p) ->
    input_create env ~for_list ~input_name ~role p
| Input_finder (for_list, input_name, role) ->
    input_finder env ~for_list ~input_name ~role
| Input_finder_find (for_list, input_name, role, sel) ->
    input_finder_find env ~for_list ~input_name ~role sel
| New_form { cancel } -> new_form env req ~cancel
| Page ref -> page env ref
| Replace id -> replace env req id
| Replace_form id -> replace_form env req id
| Update id -> update env req id
| View_fields id  -> view_fields env req id

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
