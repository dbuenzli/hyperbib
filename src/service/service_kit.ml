(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

let find_doi db doi =
  let r = Reference.find_doi (Adhoc_schema.Doi_rel.v doi) in
  Db.first db (Rel_query.Sql.of_bag' Reference.table r)

let find_doi_suggestion db doi =
  let r = Suggestion.find_doi (Adhoc_schema.Doi_rel.v doi) in
  Db.first db (Rel_query.Sql.of_bag' Suggestion.table r)

let find_dupe_doi ?(suggestion_dupe_check = true) g ~self db doi =
  let* exists = find_doi db doi |> Db.http_resp_error in
  match exists with
  | None ->
      if not suggestion_dupe_check then Ok None else
      let* exists = find_doi_suggestion db doi |> Db.http_resp_error in
      begin match exists with
      | None -> Ok None
      | Some s ->
          Ok (Some (Doi_html.warn_doi_suggestion_exists g ~self doi s))
      end
  | Some r -> Ok (Some (Doi_html.warn_doi_exists g ~self doi r))

let lookup_doi env doi =
  let doi_cache = Hyperbib_conf.doi_cache_dir (Service_env.conf env) in
  let* httpc =
    Result.map_error
      (* Bof *)
      (fun e -> Result.get_error (Http.Response.server_error_500 ~log:e ())) @@
    Result.map Option.some
      (Hyperbib_conf.http_client (Service_env.conf env))
  in
  Ok (doi, Import.Doi.get_ref httpc ~cache:doi_cache doi)

let fill_in_reference_form
    ?suggestion_dupe_check env db ~self ~cancel ~from_suggestion ~doi
  =
  match Doi.extract doi with
  | None ->
      Ok (Error (None, Uimsg.doi_extract_error doi))
  | Some doi ->
      let* doi, ref = lookup_doi env doi in
      match ref with
      | Error e -> Ok (Error (Some e, Uimsg.doi_error))
      | Ok None -> Ok (Error (None, Uimsg.doi_not_found doi))
      | Ok (Some ref) ->
          let g = Service_env.page_gen env in
          let r =
            Import.Doi.reference_of_ref ~public:false ~container_id:None ref
          in
          let* msg = find_dupe_doi ?suggestion_dupe_check g ~self db doi in
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
            Reference_html.filled_in_form
              g ~self ~cancel ~from_suggestion r ~msg ~authors ~editors
              ~container ~cites
          in
          Ok (Ok html)
