(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

(* Data lookups *)

let select_containers db ~only_public sel =
  (* FIXME only_public, FIXME Ask escape % and _ in selector, order by *)
  if String.trim sel = "" then Ok [] else
  let* cs = Db.list db (Container.select_stmt sel) in
  Ok (List.sort Container.order_by_title cs)

let get_container = Entity_service.get_entity (module Container)
let get_container_ref_count db s =
  let ref_count = Reference.container_ref_count_stmt (Container.id s) in
  let* ref_count = Db.first' db ref_count in
  Ok (Option.value ~default:0 ref_count)

let get_page_data db g s =
  let only_public = Rel_query.Bool.v (Page.Gen.only_public g) in
  let all = Reference.list ~only_public in
  let id = Container.id s in
  let refs = Reference.filter_container_id (Container.Id.v id) all in
  let* refs =
    Reference.render_data ~only_public refs db |> Db.http_resp_error
  in
  Ok refs

let get_container_for_page_ref =
  let page_url n id = Container.Url.v (Page (n, id)) in
  let page_404 = Container_html.page_404 in
  let entity_find_id_stmt = Container.find_id_stmt in
  let entity_public = Container.public in
  let entity_res_name = Container.Url.res_name in
  Entity_service.entity_for_page_ref
    ~page_url ~page_404 ~entity_find_id_stmt ~entity_public ~entity_res_name

let view_fields_resp env db req id =
  let* c = get_container db id in
  let g = Service_env.page_gen env in
  let* self = Html_kit.url_of_req_referer req in
  Ok (Page.part_response (Container_html.view_fields g c ~self))

(* Responses *)

let confirm_delete env id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* c = get_container db id in
  let* ref_count = get_container_ref_count db c in
  let g = Service_env.page_gen env in
  let confirm = Container_html.confirm_delete g c ~ref_count in
  Ok (Page.part_response confirm)

let create =
  let entity_page_url id = Container.Url.v (Page (None, id)) in
  Entity_service.create (module Container.Id) (module Container)
    ~entity_page_url

let delete =
  Entity_service.delete (module Container) ~deleted_html:Container_html.deleted

let duplicate env req src =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = [Col.Def Container.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Container.table q in
  let* dst =
    Db.insert' (module Container.Id) db
      (Container.create_cols ~ignore_id:true vs)
  in
  let* () = Db.exec' db (Container.Label.copy_applications_stmt ~src ~dst) in
  let uf = Service_env.url_fmt env in
  let headers =
    Html_kit.htmlact_redirect uf (Container.Url.v (Page (None, dst)))
  in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let duplicate_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* c = get_container db id in
  let c = Container.duplicate_data c in
  let g = Service_env.page_gen env in
  let duplicate_form = Container_html.duplicate_form g c in
  Ok (Page.part_response duplicate_form)

let edit_form env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* c = get_container db id in
  let edit_form = Container_html.edit_form (Service_env.page_gen env) c in
  Ok (Page.part_response edit_form)

let index env =
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* cs = Db.list db (Container.list_stmt ~only_public) in
  let ref_count = Reference.container_public_ref_count_stmt in
  let* ref_count =
    let add (id, _ as r) acc = Container.Id.Map.add id r acc in
    Db.fold db ref_count add Container.Id.Map.empty
  in
  let page = Container_html.index g cs ~ref_count in
  Ok (Page.response page)

let new_form env req ~cancel =
  let* () = Entity_service.check_edit_authorized env in
  let g = Service_env.page_gen env in
  let page = Container_html.new_form g Container.new' ~cancel in
  Ok (Page.response page)

let page env ref =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let only_public = Page.Gen.only_public g in
  let* c = get_container_for_page_ref db g ~only_public ref in
  let* refs = get_page_data db g c in
  let page = Container_html.page g c refs in
  Ok (Page.response page)

let _replace_form env db this =
  let* c = get_container db this in
  let* ref_count = get_container_ref_count db c in
  let g = Service_env.page_gen env in
  let replace = Container_html.replace_form g c ~ref_count in
  Ok (Page.part_response replace)

let replace_form env req this =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  _replace_form env db this

let replace env req this =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let* by =
    let* by = Entity.Url.replace_by_of_query' (module Container.Id) q in
    match by with
    | Some _ as by -> Ok by
    | None ->
        match Hquery.find_create_container q with
        | None -> Ok None
        | Some c ->
            let* cid =
              Db.insert' (module Container.Id) db
                (Container.create ~ignore_id:true c)
            in
            Ok (Some cid)
  in
  match by with
  | None ->
      (* FIXME solve that front end,
         the request should not be done if we don't have the data *)
      _replace_form env db this
  | Some by ->
      if this = by then view_fields_resp env db req this else
      let rep = Reference.replace_container_stmt ~this ~by in
      let* () = Db.exec' db rep in
      let* () = Db.exec' db (Container.delete this) in
      let uf = Service_env.url_fmt env in
      let headers = Html_kit.htmlact_redirect uf (Container.Url.v (Page (None, by))) in
      Ok (Http.Response.empty ~headers Http.Status.ok_200)

let input env ~input_name id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let* c = get_container db id in
  let c = Entity_html.container_input uf ~input_name c in
  Ok (Page.part_response c)

let input_create env ~input_name c =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let c = Entity_html.container_input_create uf ~input_name c in
  Ok (Page.part_response c)

let input_finder env ~input_name =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let uf = Page.Gen.url_fmt (Service_env.page_gen env) in
  let sel = Entity_html.container_input_finder uf ~input_name in
  Ok (Page.part_response sel)

let creatable_container_of_sel sel =
  let sel = String.trim sel in
  if sel = "" then None else
  Option.some @@ Container.make
    ~id:Container.Id.zero ~title:sel ~isbn:"" ~issn:"" ~note:"" ~private_note:""
    ~public:false ()

let input_finder_find env ~input_name sel =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let only_public = Page.Gen.only_public g in
  let* cs = select_containers db ~only_public sel in
  let creatable = creatable_container_of_sel sel in
  let sel =
    Entity_html.container_input_finder_results uf ~input_name ~creatable cs
  in
  Ok (Page.part_response sel)

let update env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = Col.[Def Container.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Container.table q in
  let* () = Db.exec' db (Container.update id vs) in
  let* c = get_container db id in
  let g = Service_env.page_gen env in
  let uf = Page.Gen.url_fmt g in
  let* refs = get_page_data db g c in
  let* self = Html_kit.url_of_req_referer req in
  let html = Container_html.view_full g c ~self refs in
  let title = Container_html.page_full_title g c in
  let headers = Html_kit.htmlact_page_location_update uf self ~title () in
  Ok (Page.part_response ~headers html)

let view_fields env req id =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  view_fields_resp env db req id

let resp r env sess req = match (r : Container.Url.t) with
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
| Input (input_name, id) -> input env ~input_name id
| Input_create (input_name, c) -> input_create env ~input_name c
| Input_finder input_name -> input_finder env ~input_name
| Input_finder_find (input_name, sel) -> input_finder_find env ~input_name sel
| Update id -> update env req id
| View_fields id  -> view_fields env req id

let v = Kurl.service Container.Url.kind resp
