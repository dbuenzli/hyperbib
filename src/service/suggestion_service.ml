(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let get_suggestion = Entity_service.get_entity (module Suggestion)

let err_doi_unspecified_msg =
  El.div ~at:[Hclass.message; Hclass.error] [El.txt Uimsg.doi_unspecified]

let err_doi_error_msg =
  El.div ~at:[Hclass.message; Hclass.error] [El.txt Uimsg.doi_error]

let err_doi_extract_error_msg doi =
  El.div ~at:[Hclass.message; Hclass.error]
    [El.txt (Uimsg.doi_extract_error doi)]

let err_doi_not_found_msg doi =
  El.div ~at:[Hclass.message; Hclass.error] [El.txt (Uimsg.doi_not_found doi)]

let suggestion_notification env id =
  if not (Service_env.suggestion_notification env) then Ok () else
  let uf = Service_env.url_fmt env in
  let url = Kurl.Fmt.url ~full:true uf (Suggestion.Url.v Index) in
  let url = String.concat "#" [url; string_of_int id] in
  let sender = Service_env.email_sender env in
  let recipient = Service_env.notification_email env in
  let bib = Page.Gen.bibliography (Service_env.page_gen env) in
  let name = Bibliography.project_short_title bib in
  let subject = String.concat " " [Uimsg.new_suggestion_on; name] in
  let body =
    Fmt.str "%s,\n\n%s\n\n  %s\n\n---"
      Uimsg.hello Uimsg.someone_made_new_suggestion_here url
    (* TODO add link how to deactivate. *)
  in
  Email.send ~sender ~recipient ~subject ~body

let lookup_doi db env req s =
  let* self = Html_kit.url_of_req_referer req in
  match Doi.extract (Suggestion.doi s) with
  | None ->
      let msg = err_doi_extract_error_msg (Suggestion.doi s) in
      let e = "Could not extract DOI" in
      Ok (Suggestion.doi s, Suggestion.suggestion s, Some msg, Some e)
  | Some doi ->
      let* doi, ref = Service_kit.lookup_doi env doi in
      let g = Service_env.page_gen env in
      match ref with
      | Error e ->
          let msg = err_doi_error_msg in
          Ok (doi, Suggestion.suggestion s, Some msg, Some e)
      | Ok None ->
          Ok (doi, Suggestion.suggestion s,
              Some (err_doi_not_found_msg doi), None)
      | Ok (Some ref) ->
          let suggestion = Import.Doi.ref_to_short_text_citation ref in
          let* msg = Service_kit.find_dupe_doi g ~self db doi in
          Ok (doi, suggestion, msg, None)

let honeypot_error fill =
  let explain = Fmt.str "Bot honeypot triggered with fill: %s" fill in
  let html =
    let msg = El.txt Uimsg.system_thinks_you_are_a_bot in
    El.p ~at:[Hclass.message; Hclass.error] [msg]
  in
  Error (Page.part_response ~explain ~status:Http.Status.forbidden_403 html)

let validate_suggestion db env req s =
  let g = Service_env.page_gen env in
  if String.trim (Suggestion.suggestion s) <> "" then Ok (Ok s) else
  if String.trim (Suggestion.doi s) = "" then
    let msg = Suggestion_html.need_a_doi_or_suggestion in
    Ok (Error (Suggestion_html.suggest_form ~msg g s))
  else
  let* doi, suggestion, msg, explain = lookup_doi db env req s in
  let s =
    let comment = Suggestion.comment s and email = Suggestion.email s in
    Suggestion.v ~id:0 ~timestamp:0 ~doi ~suggestion ~comment ~email ()
  in
  match msg with
  | None -> Ok (Ok s)
  | Some msg ->
      Ok (Error (Suggestion_html.suggest_form ~force_rescue:true ~msg g s))

let suggestion_of_req req =
  let* q = Http.Request.to_query req in
  let honey = Hquery.key Suggestion_html.bot_honeypot_field Hquery.string in
  let* fill = Hquery.get honey q in
  if fill <> "" then honeypot_error fill else
  let* doi = Hquery.get_col Suggestion.doi' q in
  let* suggestion = Hquery.get_col Suggestion.suggestion' q in
  let* comment = Hquery.get_col Suggestion.comment' q in
  let* email =
    Hquery.get (Hquery.key Suggestion_html.email_field Hquery.string) q
  in
  let timestamp =
    Option.value ~default:0 @@ Ptime.Span.to_int_s @@
    Ptime.to_span (Ptime_clock.now ())
  in
  Ok (Suggestion.v ~id:0 ~timestamp ~doi ~suggestion ~comment ~email ())

(* Responses *)

let confirm_delete env id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_suggestion db id in
  let g = Service_env.page_gen env in
  let confirm = Suggestion_html.confirm_delete g s in
  Ok (Page.part_response confirm)

let create env req =
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* s = suggestion_of_req req in
  let* v = validate_suggestion db env req s in
  match v with
  | Error html -> Ok (Page.part_response html)
  | Ok s ->
      let* id = Db.insert' db (Suggestion.create ~ignore_id:true s) in
      let uf = Service_env.url_fmt env in
      let () = suggestion_notification env id |> Log.if_error ~use:() in
      let redirect = Suggestion.Url.v (Page {id; created = true}) in
      let headers = Html_kit.htmlact_redirect uf redirect in
      Ok (Http.Response.empty ~headers Http.Status.ok_200)

let delete env id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* c = get_suggestion db id in
  let* () = Db.exec' db (Suggestion.delete id) in
  let headers = Http.Headers.def Htmlact.reload "true" Http.Headers.empty in
  Ok (Page.part_response ~headers (El.splice []))

let fill_in env req =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let* s = suggestion_of_req req in
  let doi = Suggestion.doi s in
  let* doi, suggestion, msg, explain =
    if doi = ""
    then Ok (doi, Suggestion.suggestion s, Some err_doi_unspecified_msg, None)
    else lookup_doi db env req s
  in
  let s =
    let comment = Suggestion.comment s and email = Suggestion.email s in
    Suggestion.v ~id:0 ~timestamp:0 ~doi ~suggestion ~comment ~email ()
  in
  let html = Suggestion_html.suggest_form ~force_rescue:true ?msg g s in
  Ok (Page.part_response ?explain html)

let view_fields env req id =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let* s = get_suggestion db id in
  let g = Service_env.page_gen env in
  let* self = Html_kit.url_of_req_referer req in
  Ok (Page.part_response (Suggestion_html.view_fields g s ~self))

let integrate env req id =
  let* () = Entity_service.check_edit_authorized env in
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let* s = get_suggestion db id in
  let self = Suggestion.Url.v (Page { id; created = false}) in
  let suggs = Suggestion.Url.v Index in
  let cancel = Some (Kurl.Fmt.url (Service_env.url_fmt env) suggs) in
  let* explain, form = match Suggestion.doi s with
  | "" ->
      let g = Service_env.page_gen env in
      Ok (None, Service_kit.empty_reference_form g ~self ~cancel)
  | doi ->
      Service_kit.fill_in_reference_form ~no_suggestion_dupe_check:true
        env db ~self ~cancel doi
  in
  let g = Service_env.page_gen env in
  let page = Suggestion_html.integrate g s ~form in
  Ok (Page.response page)

let created env req id =
  Service_env.with_db_transaction' `Immediate env @@ fun db ->
  let g = Service_env.page_gen env in
  let* s = Db.first' db (Suggestion.find_id_stmt id) in
  match s with
  | None ->
      let page_url = Suggestion.Url.v (Page {id; created = true}) in
      let page = Suggestion_html.page_404 g ~self:page_url in
      Error (Page.response_404 page)
  | Some s ->
      let page = Suggestion_html.created g s in
      Ok (Page.response page)

let index env =
  Service_env.with_db_transaction' `Deferred env @@ fun db ->
  let g = Service_env.page_gen env in
  let* ss = Db.list' db Suggestion.list_stmt in
  let is_full = List.length ss > Service_env.max_pending_suggestions env in
  let page = Suggestion_html.index g ss ~is_full in
  Ok (Page.response page)

let resp r env user req = match (r : Suggestion.Url.t) with
| Create -> create env req
| Delete id -> delete env id
| Fill_in -> fill_in env req
| Index -> index env
| Page {id; created = true} -> created env req id
| Page {id; created = false} -> integrate env req id
| Confirm_delete id -> confirm_delete env id
| View_fields id -> view_fields env req id

let v = Kurl.service Suggestion.Url.kind resp
