(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

(* Data lookups *)

let get_doc = Entity_service.get_entity (module Reference.Doc)
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
  let ref = Reference.find_id (Reference.Id.v (Reference.id r)) in
  Reference.render_data ~only_public ref db

let get_page_data db g r =
  (* The render_data calls could be streamlined to a single one. *)
  Db.http_resp_error @@
  let only_public = Page.Gen.only_public g in
  let only_public = Rel_query.Bool.v only_public in
  let rid = Reference.Id.v (Reference.id r) in
  let ref = Reference.find_id rid in
  let* render_data = Reference.render_data ~only_public ref db in
  let cites = Reference.find_dois (Reference.dois_cited rid) in
  let* cites = Reference.render_data ~only_public cites db in
  let cited_by = match Reference.doi r with
  | None -> Bag.empty
  | Some doi -> Reference.citing_doi (Rel_query.Text.v doi)
  in
  let* cited_by = Reference.render_data ~only_public cited_by db in
  Ok (render_data, cites, cited_by)

let view_fields_resp ?authors_ui env db req id =
  let* r = get_reference db id in
  let g = Service_env.page_gen env in
  let* self = Html_kit.url_of_req_referer req in
  let rid = Reference.Id.v (Reference.id r) in
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
      (fun e ->
         Result.get_error (Http.Response.bad_request_400 ~log:e ())) @@
    let* bare = Kurl.Bare.of_req_referer req in
    Ok (Entity.Url.cancel_url_of_query (Kurl.Bare.query bare))
  in
  (* FIXME replace by let* self = Hfrag.url_of_req_referer req in *)
  let self (* XXX *) = Reference.Url.v (New_form { cancel }) in
  let* log, part =
    Service_kit.fill_in_reference_form env db ~self ~cancel doi
  in
  Ok (Page.part_response ?log part)

let change_authors_publicity env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let* is_undo = Hquery.find_first Hquery.key_is_undo ~none:false q in
  let* authors_ui = match is_undo with
  | true ->
      let* ids = Hquery.find_ids (module Person.Id) ~uniquify:true "undo" q in
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
      let* cid =
        Db.insert' (module Container.Id) db (Container.create ~ignore_id:true c)
      in
      Ok (Col.Value (Reference.container', Some cid) :: vs)

let authors_editors_maybe_create db q =
  let create created p =
    let is_p (p', id) = Person.created_equal p' p in
    match List.find_opt is_p created with
    | Some (_, id) -> Ok (id, created)
    | None ->
        let* pid =
          Db.insert' (module Person.Id) db (Person.create ~ignore_id:true p)
        in
        Ok (pid, (p, pid) :: created)
  in
  let rec ids created acc = function
  | [] -> Ok (Hquery.uniquify_ids (module Person.Id) (List.rev acc), created)
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
  let ignore = [Col.Def Reference.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Reference.table q in
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
    Hquery.find_ids (module Subject.Id) ~uniquify:true key q
  in
  let* aids, eids = authors_editors_maybe_create db q in
  let cites = Hquery.find_cites q in
  let* vs = maybe_create_container db vs q in
  let created = Col.Value (Reference.created_ptime_s', Unix.gettimeofday ()) in
  let vs = created :: vs in
  let* id =
    Db.insert' (module Reference.Id) db
      (Reference.create_cols ~ignore_id:true vs)
  in
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
  let headers = Html_kit.htmlact_redirect uf (entity_page_url id) in
  Ok (Http.Response.empty ~headers Http.Status.ok_200)

let update env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* q = Http.Request.to_query req in
  let ignore = [Col.Def Reference.id'] in
  let* vs = Hquery.careless_find_table_cols ~ignore Reference.table q in
  let vs = match Hquery.find_date q with
  | Error _ (* FIXME form validation, which we also need to poperly type
               dois. *) -> vs
  | Ok d ->
      let y, md = Reference.col_values_for_date d in
      y :: md :: vs
  in
  let uniquify = true in
  let* sids =
    let key = Reference.Subject.(Hquery.key_for_rel table subject') in
    Hquery.find_ids (module Subject.Id) ~uniquify key q
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
  (* FIXME, commit 04a09ae1036 improved certain fragments but before we
     had the following hich has the advantage that the URL chanegs
     if we update the title.
     let self = Reference.Url.page r (* assume comes from that page *)
  *)
  let* self = Html_kit.url_of_req_referer req in
  let title = Reference_html.page_full_title g r in
  let html = Reference_html.view_full g ~self r ~render_data ~cites ~cited_by in
  let headers = Html_kit.htmlact_page_location_update uf self ~title () in
  Ok (Page.part_response ~headers html)

let doc env request (_, ref_id) docid =
  Service_env.with_db' env @@ fun db ->
  let* doc = get_doc db docid in
  let is_private = not (Reference.Doc.public doc) in
  let see_private_data = User.Caps.see_private_data (Service_env.caps env) in
  if not see_private_data && is_private (* 404 to avoid probing *)
  then Http.Response.not_found_404 ~log:"Not authorized" () else
  let file =
    let* blobstore = Cli_kit.Conf.blobstore (Service_env.conf env) in
    let* id = Blobstore.Key.of_text (Reference.Doc.blob_key doc) in
    Blobstore.find id blobstore
  in
  let* filename =
    let name = String.trim (Reference.Doc.name doc) in
    if name <> "" then Ok name else
    (* TODO we could have a better name than the doi here. *)
    let* ref = get_reference db ref_id in
    let doi = Reference.doi ref in
    match doi with
    | None | Some "" -> Ok "doc"
    | Some doi -> Ok doi
  in
  let media_type = Reference.Doc.media_type doc in
  let ext = Media_type.to_file_ext media_type in
  let filename =
    if String.ends_with ~suffix:ext filename then filename else filename ^ ext
  in
  (* TODO escaping
     TODO Webs_fs add something for content disposition. *)
  let content_disposition = Fmt.str "inline; filename=\"%s\"" filename in
  match file with
  | Error e -> Http.Response.server_error_500 ~log:e ()
  | Ok None -> Http.Response.not_found_404 ()
  | Ok (Some file) ->
      let* response = Webs_fs.send_file request (Fpath.to_string file) in
      let forever = "public, max-age=31536000, immutable" in
      let hs = Http.Headers.(def cache_control) forever Http.Headers.empty in
      let hs = Http.Headers.(def content_type) media_type hs in
      let hs = Http.Headers.(def content_disposition) content_disposition hs in
      Ok (Http.Response.override_headers ~by:hs response)

let view_fields app req id =
  Service_env.with_db_transaction' `Deferred app @@ fun db ->
  view_fields_resp app db req id

let resp r env sess request = match (r : Reference.Url.t) with
| Change_authors_publicity id -> change_authors_publicity env request id
| Confirm_delete id -> confirm_delete env id
| Create -> create env request
| Delete id -> delete env id
| Doc (ref, doc_id) -> doc env request ref doc_id
| Edit_form id -> edit_form env request id
| Fill_in_form doi -> fill_in_form env request doi
| Index -> index env
| New_form { cancel } -> new_form env request ~cancel
| Page ref -> page env ref
| Update id -> update env request id
| View_fields id  -> view_fields env request id



let v = Kurl.service Reference.Url.kind resp
