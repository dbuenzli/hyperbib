(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let find_doi db doi =
  let r = Reference.find_doi (Rel_query.Text.v doi) in
  Db.first db (Rel_query.Sql.of_bag' Reference.table r)

let find_doi_suggestion db doi =
  let r = Suggestion.find_doi (Rel_query.Text.v doi) in
  Db.first db (Rel_query.Sql.of_bag' Suggestion.table r)

let find_dupe_doi ?(no_suggestion_dupe_check = false) g ~self db doi =
  let* exists = find_doi db doi |> Db.http_resp_error in
  match exists with
  | None ->
      if no_suggestion_dupe_check then Ok None else
      let* exists = find_doi_suggestion db doi |> Db.http_resp_error in
      begin match exists with
      | None -> Ok None
      | Some s ->
          Ok (Some (Doi_html.warn_doi_suggestion_exists g ~self doi s))
      end
  | Some r -> Ok (Some (Doi_html.warn_doi_exists g ~self doi r))

let lookup_doi env doi =
  let doi_cache = Cli_kit.Conf.doi_cache_dir (Service_env.conf env) in
  let* httpc =
    Result.map_error
      (* Bof *)
      (fun e ->
         Result.get_error (Http.Response.server_error_500 ~explain:e ())) @@
    Result.map Option.some
      (Cli_kit.Conf.http_client (Service_env.conf env))
  in
  Ok (doi, Import.Doi.get_ref httpc ~cache:doi_cache doi)

let resp_err_doi_not_found g ~self ~cancel doi = (* move to reference_html *)
  (* FIXME it would nice to preserve the form user input here *)
  let at = [Hclass.message; Hclass.error] in
  let msg = El.div ~at [El.txt (Uimsg.doi_not_found doi)] in
  Reference_html.filled_in_form g
    Reference.new' ~self ~cancel ~msg ~authors:[] ~editors:[] ~container:None
    ~cites:[]

let empty_reference_form ?(msg = El.void) g ~self ~cancel =
  Reference_html.filled_in_form g Reference.new'
    ~self ~cancel ~msg ~authors:[] ~editors:[] ~container:None ~cites:[]

let resp_err_doi_error g ~self ~cancel = (* move to reference_html *)
  (* FIXME it would nice to preserve the form user input here *)
  let at = [Hclass.message; Hclass.error] in
  let msg = El.p ~at [El.txt Uimsg.doi_error]in
  empty_reference_form ~msg g ~self ~cancel

let fill_in_reference_form ?no_suggestion_dupe_check env db ~self ~cancel doi =
  let g = Service_env.page_gen env in
  let* doi, ref = lookup_doi env doi in
  match ref with
  | Error e -> Ok (Some e, resp_err_doi_error g ~self ~cancel)
  | Ok None -> Ok (None, resp_err_doi_not_found g ~self ~cancel doi)
  | Ok (Some ref) ->
      let r =
        Import.Doi.reference_of_ref ~public:false ~container_id:None ref
      in
      let* msg = find_dupe_doi ?no_suggestion_dupe_check g ~self db doi in
      let msg = Option.value ~default:El.void msg in
      let* container =
        Import.Doi.get_container ~create_public:false db ref
        |> Db.http_resp_error
      in
      let* authors, editors =
        Import.Doi.authors_editors_of_ref ~create_public:false db ref
        |> Db.http_resp_error
      in
      let cites = Import.Doi.cites_of_ref ref in
      let html =
        Reference_html.filled_in_form g ~cancel r
          ~self ~msg ~authors ~editors ~container ~cites
      in
      Ok (None, html)
