(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open B0_json

type partial_date = int * (int * int option) option
let partial_date =
  let parse_date = function
  | [y; m; d] -> Jsonq.succeed (y, Some (m, Some d))
  | [y; m] -> Jsonq.succeed (y, Some (m, None))
  | [y] -> Jsonq.succeed (y, None)
  | _ -> Jsonq.fail "cannot parse partial date"
  in
  Jsonq.(mem "date-parts" (bind (nth 0 (array int)) parse_date))

module Contributor = struct
  let family = Jsonq.(mem "family" string)
  let given = Jsonq.(opt_mem ~absent:None "given" (some string))
  let orcid = Jsonq.(opt_mem ~absent:None "ORCID" (some string))
end

module Reference = struct
  let doi = Jsonq.(opt_mem ~absent:None "DOI" (some string))
end

let string_array = (* It seems we also get them as simple strings *)
  let string = Jsonq.(map (fun s -> [s]) string) in
  let array = Jsonq.(array string) in
  Jsonq.partial_fold ~string ~array ()

module Work = struct
  let author q = Jsonq.(opt_mem ~absent:None "author" (some (array q)))
  let abstract = Jsonq.(opt_mem ~absent:None "abstract" (some string))
  let container_title =
    Jsonq.(opt_mem "container-title" ~absent:None (some string_array))

  let doi = Jsonq.(mem "DOI" string)
  let editor q = Jsonq.(opt_mem ~absent:None "editor" (some (array q)))
  let issn = Jsonq.(opt_mem ~absent:None "ISSN" (some (array string)))
  let isbn = Jsonq.(opt_mem ~absent:None "ISBN" (some (array string)))
  let issue = Jsonq.(opt_mem ~absent:None "issue" (some string))
  let issued = Jsonq.(mem "issued" partial_date)
  let page = Jsonq.(opt_mem ~absent:None "page" (some string))
  let publisher = Jsonq.(mem "publisher" string)
  let reference q = Jsonq.(opt_mem ~absent:None "reference" (some (array q)))
  let subject = Jsonq.(opt_mem ~absent:None "subject" (some (array string)))
  let title = Jsonq.(mem "title" string_array)
  let type' = Jsonq.(mem "type" string)
  let volume = Jsonq.(opt_mem ~absent:None "volume" (some string))
end

let rec for_doi httpc ~cache doi =
  let doi_to_fname doi = String.map (function '/' -> '_' | c -> c) doi in
  let doi_file = Fpath.(cache / doi_to_fname doi + ".json") in
  let* exists = Os.File.exists doi_file in
  match exists with
  | true ->
      let* contents = Os.File.read doi_file in
      Result.map Option.some @@
      Json.of_string ~file:(Fpath.to_string doi_file) contents
  | false ->
      let* httpc = match httpc with
      | None -> Fmt.error "No cached metadata for %s" doi
      | Some httpc -> Ok httpc
      in
      let content_type = Doi.json in
      let* json = Doi.resolve_to_content_type ~content_type httpc doi in
      match json with
      | None -> Ok None
      | Some json ->
          let* () = Os.File.write ~make_path:true ~force:true doi_file json in
          for_doi None ~cache doi

let types = [
  (* As found in https://api.crossref.org/v1/types
     jq '.message.items[] | [.id, .label] | @csv' | \
     sed 's/\\"/"/g; s/""$/";/g; s/^""/"/g' *)
  "book","Book";
  "book-chapter","Book Chapter";
  "book-part","Book Part";
  "book-section","Book Section";
  "book-series","Book Series";
  "book-set","Book Set";
  "book-track","Book Track";
  "component","Component";
  "dataset","Dataset";
  "dissertation","Dissertation";
  "edited-book","Edited Book";
  "journal","Journal";
  "journal-article","Journal Article";
  "journal-issue","Journal Issue";
  "journal-volume","Journal Volume";
  "monograph","Monograph";
  "other","Other";
  "peer-review","Peer Review";
  "posted-content","Posted Content";
  "proceedings","Proceedings";
  "proceedings-article","Proceedings Article";
  "proceedings-series","Proceedings Series";
  "reference-book","Reference Book";
  "reference-entry","Reference Entry";
  "report","Report";
  "report-series","Report Series";
  "standard","Standard";
  "standard-series","Standard Series"; ]
