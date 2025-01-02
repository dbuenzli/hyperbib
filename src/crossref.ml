(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax

type partial_date = int * (int * int option) option

let partial_date_jsont  =
  let parse_date = function
  | [y; m; d] :: _ -> (y, Some (m, Some d))
  | [y; m] :: _ -> (y, Some (m, None))
  | [y] :: _ -> (y, None)
  | _ -> Jsont.Error.msg Jsont.Meta.none "cannot parse partial date"
  in
  Jsont.Object.map parse_date
  |> Jsont.Object.mem "date-parts" Jsont.(list (list int))
  |> Jsont.Object.finish

module Contributor = struct
  type t =
    { family : string;
      given : string;
      orcid : string; }

  let equal c0 c1 =
    (String.equal c0.orcid c1.orcid) ||
    (String.equal c0.family c1.family && String.equal c0.given c1.given)

  let jsont =
    Jsont.Object.map (fun family given orcid -> { family; given; orcid })
    |> Jsont.Object.mem "family" Jsont.string
    |> Jsont.Object.mem "given" Jsont.string ~dec_absent:""
    |> Jsont.Object.mem "ORCID" Jsont.string ~dec_absent:""
    |> Jsont.Object.finish
end

module Reference = struct
  type t = { doi : Doi.t option }
  let jsont =
    Jsont.Object.map (fun doi -> { doi })
    |> Jsont.Object.opt_mem "DOI" Doi.jsont
    |> Jsont.Object.finish
end

module License = struct
  type t =
    { content_version : string;
      start : partial_date;
      url : string; }

  let make content_version start url =
    { content_version; start; url }

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "content-version" Jsont.string
    |> Jsont.Object.mem "start" partial_date_jsont
    |> Jsont.Object.mem "URL" Jsont.string
    |> Jsont.Object.finish
end

module Ressource_link = struct
  type t =
    { intended_application : string;
      content_version : string;
      url : string;
      content_type : string option; }

  let make intended_application content_version url content_type =
    { intended_application; content_version; url; content_type}

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "intended-application" Jsont.string
    |> Jsont.Object.mem "content-version" Jsont.string
    |> Jsont.Object.mem "URL" Jsont.string
    |> Jsont.Object.opt_mem "content-type" Jsont.string
    |> Jsont.Object.finish
end

module Work = struct
  type t =
    { author : Contributor.t list;
      abstract : string;
      container_title : string list;
      doi : Doi.t;
      editor : Contributor.t list;
      issn : string list;
      isbn : string list;
      issue : string option;
      issued : partial_date;
      license : License.t list;
      link : Ressource_link.t list;
      page : string option;
      publisher : string;
      reference : Reference.t list;
      subject : string list;
      title : string list;
      type' : string;
      volume : string option; }

  let make
      author abstract container_title doi editor issn isbn issue issued
      license link page publisher reference subject title type' volume
    =
    { author; abstract; container_title; doi; editor; issn; isbn; issue;
      issued; license; link; page; publisher; reference; subject; title; type';
      volume; }

  let string_or_string_array = (* It seems we also get them as simple strings *)
    let dec_array = Jsont.(list string) in
    let dec_string = Jsont.map ~dec:(fun s -> [s]) Jsont.string in
    Jsont.any ~dec_string ~dec_array ()

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "author" Jsont.(list Contributor.jsont) ~dec_absent:[]
    |> Jsont.Object.mem "abstract" Jsont.string ~dec_absent:""
    |> Jsont.Object.mem "container-title" string_or_string_array ~dec_absent:[]
    |> Jsont.Object.mem "DOI" Doi.jsont
    |> Jsont.Object.mem "editor" Jsont.(list Contributor.jsont) ~dec_absent:[]
    |> Jsont.Object.mem "ISSN" Jsont.(list string) ~dec_absent:[]
    |> Jsont.Object.mem "ISBN" Jsont.(list string) ~dec_absent:[]
    |> Jsont.Object.opt_mem "issue" Jsont.string
    |> Jsont.Object.mem "issued" partial_date_jsont
    |> Jsont.Object.mem "license" Jsont.(list License.jsont) ~dec_absent:[]
    |> Jsont.Object.mem "link" Jsont.(list Ressource_link.jsont) ~dec_absent:[]
    |> Jsont.Object.opt_mem "page" Jsont.string
    |> Jsont.Object.mem "publisher" Jsont.string
    |> Jsont.Object.mem "reference" Jsont.(list Reference.jsont) ~dec_absent:[]
    |> Jsont.Object.mem "subject" Jsont.(list string)
    |> Jsont.Object.mem "title" string_or_string_array
    |> Jsont.Object.mem "type" Jsont.string
    |> Jsont.Object.opt_mem "volume" Jsont.string
    |> Jsont.Object.finish
end

let rec for_doi httpc ~cache doi =
  let doi_file = Fpath.(cache / Doi.as_filename doi + ".json") in
  let* exists = Os.File.exists doi_file in
  match exists with
  | true ->
      let* contents = Os.File.read doi_file in
      let file = Fpath.to_string doi_file in
      Result.map Option.some @@
      Jsont_bytesrw.decode_string ~locs:true ~file Work.jsont contents
  | false ->
      let* httpc = match httpc with
      | None -> Fmt.error "No cached metadata for %a" Doi.pp doi
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
