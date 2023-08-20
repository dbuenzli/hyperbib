(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax
open Rel

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
  Db.http_resp_error @@
  let only_public = Page.Gen.only_public g in
  let only_public = Rel_query.Bool.v only_public in
  let rid = Rel_query.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  Reference.render_data ~only_public ref db

let get_page_data db g r =
  (* The render_data calls could be streamlined to a single one. *)
  Db.http_resp_error @@
  let only_public = Page.Gen.only_public g in
  let only_public = Rel_query.Bool.v only_public in
  let rid = Rel_query.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  let* render_data = Reference.render_data ~only_public ref db in
  let cites = Reference.find_dois (Reference.dois_cited rid) in
  let* cites = Reference.render_data ~only_public cites db in
  let cited_by = match Reference.doi r with
  | "" -> Bag.empty
  | doi -> Reference.citing_doi (Rel_query.Text.v doi)
  in
  let* cited_by = Reference.render_data ~only_public cited_by db in
  Ok (render_data, cites, cited_by)

let view_fields_resp ?authors_ui env db req id =
  let* r = get_reference db id in
  let g = Service_env.page_gen env in
  let* self = Hfrag.url_of_req_referer req in
  let rid = Rel_query.Int.v (Reference.id r) in
  let ref = Reference.find_id rid in
  let only_public = Page.Gen.only_public g in
  let only_public = Rel_query.Bool.v only_public in
  let* render_data =
    Reference.render_data ~only_public ref db |> Db.http_resp_error
  in
  let part = Reference_html.view_fields ?authors_ui g ~self r ~render_data in
  Ok (Page.part_response part)

(* Reponses *)

let confirm_delete app id =
  let* () = Entity_service.check_edit_authorized app in
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  let* r = get_reference db id in
  let g = Service_env.page_gen app in
  let confirm = Reference_html.confirm_delete g r in
  Ok (Page.part_response confirm)

let delete =
  Entity_service.delete (module Reference) ~deleted_html:Reference_html.deleted

let edit_form app req id =
  let* () = Entity_service.check_edit_authorized app in
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  let* r = get_reference db id in
  let g = Service_env.page_gen app in
  let* render_data = get_reference_data db g r in
  let edit_form = Reference_html.edit_form g r ~render_data in
  Ok (Page.part_response edit_form)

let index app =
  Service_env.with_db_transaction `Deferred app @@ fun db ->
  let g = Service_env.page_gen app in
  let only_public = Rel_query.Bool.v (Page.Gen.only_public g) in
  let refs = Reference.list ~only_public in
  let* render_data = Reference.render_data ~only_public refs db in
  let page = Reference_html.index g render_data in
  Ok (Page.response page)

let new_form app req ~cancel =
  let* () = Entity_service.check_edit_authorized app in
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  let g = Service_env.page_gen app in
  let page = Reference_html.new_form g Reference.new' ~cancel in
  Ok (Page.response page)

let fill_in_form env req doi =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* cancel =
    Result.map_error
      (* Bof *)
      (fun e -> Result.get_error (Http.Response.bad_request_400 ~explain:e ())) @@
    let* bare = Kurl.Bare.of_req_referer req in
    Ok (Entity.Url.cancel_url_of_query (Kurl.Bare.query bare))
  in
  (* FIXME replace by let* self = Hfrag.url_of_req_referer req in *)
  let self (* XXX *) = Reference.Url.v (New_form { cancel }) in
  let* explain, part =
    Service_block.fill_in_reference_form env db ~self ~cancel doi
  in
  Ok (Page.part_response ?explain part)

let change_authors_publicity env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let* is_undo = Hquery.find_first Hquery.key_is_undo ~none:false q in
  let* authors_ui = match is_undo with
  | true ->
      let* ids = Hquery.find_ids ~uniquify:true "undo" q in
      let cs = [Col.Value (Person.public', false)] in
      let upd id = Db.exec' db (Person.update id cs) in
      let* () = List.iter_stop_on_error upd ids in
      Ok None
  | false ->
      (* FIXME we need to work on rel to streamline this *)
      let* ids = Db.list' db (Reference.author_ids_stmt id) in
      let ps = Rel_query.Sql.of_bag' Person.table (Person.find_id_list ids) in
      let* ps = Db.list' db ps in
      let ps = List.filter (Fun.negate Person.public) ps in
      let cs = [Col.Value (Person.public', true)] in
      let upd p = Db.exec' db (Person.update (Person.id p) cs) in
      let* () = List.iter_stop_on_error upd ps in
      let ids = List.map Person.id ps in
      let uf = Service_env.url_fmt env in
      Ok (Some (Reference_html.undo_make_all_authors_public_button uf id ~ids))
  in
  view_fields_resp ?authors_ui env db req id

let page app ref =
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  let g = Service_env.page_gen app in
  let only_public = Page.Gen.only_public g in
  let* r = get_reference_of_page_ref db g ~only_public ref in
  let* render_data, cites, cited_by = get_page_data db g r in
  let page = Reference_html.page g r ~render_data ~cites ~cited_by in
  Ok (Page.response page)

let maybe_create_container db vs q =
  match Hquery.find_create_container q with
  | None -> Ok vs
  | Some c ->
      let* cid = Db.insert' db (Container.create ~ignore_id:true c) in
      Ok (Col.Value (Reference.container', Some cid) :: vs)

let authors_editors_maybe_create db q =
  let create created p =
    let is_p (p', id) = Person.created_equal p' p in
    match List.find_opt is_p created with
    | Some (_, id) -> Ok (id, created)
    | None ->
        let* pid = Db.insert' db (Person.create ~ignore_id:true p) in
        Ok (pid, (p, pid) :: created)
  in
  let rec ids created acc = function
  | [] -> Ok (Hquery.uniquify_ids (List.rev acc), created)
  | `Id i :: ps -> ids created (i :: acc) ps
  | `To_create p :: ps ->
      match create created p with
      | Error _ as e -> e
      | Ok (i, created) -> ids created (i :: acc) ps
  in
  let* authors, editors = Hquery.find_create_contributors q in
  let* aids, created = ids [] [] authors in
  let* eids, _ = ids created [] editors in
  Ok (aids, eids)

let create app req = (* create and update are very similar factor out a bit. *)
  let* () = Entity_service.check_edit_authorized app in
  let entity_page_url id = Reference.Url.v (Page (None, id)) in
  Service_env.with_db_transaction' `Immediate app @@ fun db ->
  let* q = Http.Request.to_query req in
  let* vs =
    Hquery.careless_find_table_cols ~ignore:[Col.V Reference.id']
      Reference.table q
  in
  let vs = match Hquery.find_date q with
  | Error _ (* FIXME form validation *) -> vs
  | Ok d ->
      let y, md = Reference.col_values_for_date d in
      y :: md :: vs
  in
  let vs =
    (* These things are not set in the ui but we have an integrity constraint *)
    Col.Value (Reference.abstract', "") :: vs
  in
  let* sids =
    let key = Reference.Subject.(Hquery.key_for_rel table subject') in
    Hquery.find_ids ~uniquify:true key q
  in
  let* aids, eids = authors_editors_maybe_create db q in
  let cites = Hquery.find_cites q in
  let* vs = maybe_create_container db vs q in
  let* id = Db.insert' db (Reference.create_cols ~ignore_id:true vs) in
  let* () =
    Reference.Subject.set_list ~reference:id sids db |> Db.http_resp_error in
  let* () =
    Reference.Contributor.set_list ~reference:id ~authors:aids ~editors:eids db
    |> Db.http_resp_error
  in
  let* () =
    Reference.Cites.set_list ~reference:id ~dois:cites db
    |> Db.http_resp_error
  in
  let uf = Service_env.url_fmt app in
  let headers = Hfrag.htmlact_redirect uf (entity_page_url id) in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let update env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = [Col.V Reference.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Reference.table q in
  let vs = match Hquery.find_date q with
  | Error _ (* FIXME form validation *) -> vs
  | Ok d ->
      let y, md = Reference.col_values_for_date d in
      y :: md :: vs
  in
  let uniquify = true in
  let* sids =
    let key = Reference.Subject.(Hquery.key_for_rel table subject') in
    Hquery.find_ids ~uniquify key q
  in
  let* aids, eids = authors_editors_maybe_create db q in
  let* vs = maybe_create_container db vs q in
  let* () = Db.exec' db (Reference.update id vs) in
  let* () =
    Reference.Subject.set_list ~reference:id sids db |> Db.http_resp_error
  in
  let* () =
    Reference.Contributor.set_list ~reference:id ~authors:aids ~editors:eids db
    |> Db.http_resp_error
  in
  let* r = get_reference db id in
  let g = Service_env.page_gen env in
  let* render_data, cites, cited_by = get_page_data db g r in
  let uf = Page.Gen.url_fmt g in
  let* self = Hfrag.url_of_req_referer req in
  let title = Reference_html.page_full_title g r in
  let html = Reference_html.view_full g ~self r ~render_data ~cites ~cited_by in
  let headers = Hfrag.htmlact_page_location_update uf self ~title () in
  Ok (Page.part_response ~headers html)

let view_fields app req id =
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r env sess req = match (r : Reference.Url.t) with
| Change_authors_publicity id -> change_authors_publicity env req id
| Confirm_delete id -> confirm_delete env id
| Create -> create env req
| Delete id -> delete env id
| Edit_form id -> edit_form env req id
| Fill_in_form doi -> fill_in_form env req doi
| Index -> index env
| New_form { cancel } -> new_form env req ~cancel
| Page ref -> page env ref
| Update id -> update env req id
| View_fields id  -> view_fields env req id

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
