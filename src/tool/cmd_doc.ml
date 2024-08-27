(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

let try_doi_resolvers httpc ~doi_resolvers ~media_type ~doi =
  let not_found doi =
    Fmt.error "@[<v>No document found for %a.@,\
               Invoke with %a for more information."
      (Fmt.code' Doi.pp) doi Fmt.code "-v"
  in
  let rec loop = function
  | [] -> not_found doi
  | resolver :: rs ->
      Log.info (fun m -> m "Trying %s" (Doi.as_url ~resolver doi));
      match Doi.to_document httpc ~resolver ~media_type doi with
      | Error e -> Log.info (fun m -> m "%s" e); loop rs
      | Ok (url, doc) -> Ok (resolver, url, doc)
  in
  loop doi_resolvers



let fill ~dry_run conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@

  Ok Cli_kit.Exit.ok


let fetch ~doi_resolvers ~media_type ~doi ~outf conf =
  Log.if_error ~use:Cli_kit.Exit.some_error @@
  let* httpc = Cli_kit.Conf.http_client conf in
  (* XXX we should extract and use a resolver from doi if there is one *)
  let* doi = Doi.of_string doi in
  let doi_resolvers = Doi.default_resolver :: doi_resolvers in
  Log.info (fun m -> m "Looking up %a" Doi.pp doi);
  let* resolver, _url, doc =
    try_doi_resolvers httpc ~doi_resolvers ~media_type ~doi
  in
  Log.info (fun m -> m "Doc found with %s. Writing %a" resolver Fpath.pp outf);
  let* doc = Http.Body.to_string doc in
  let* () = Os.File.write outf ~force:true ~make_path:true doc in
  Ok Cli_kit.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let fill_cmd =
  let doc = "fill" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) tries to fill-in the document store for those \
          references that do not have an associated document";
    ]
  in
  Cli_kit.cmd_with_conf "fill" ~doc ~man @@
  let+ dry_run =
    let doc = "Do not lookup document. Only report those references that \
               lack documents."
    in
    Arg.(value & flag & info ["c"; "dry-run"] ~doc)
  in
  fill ~dry_run


let fetch_cmd =
  let doc = "fetch" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) tries to fetch a document corresponding to a \
          standard identifier";
      `Pre "$(iname) $(b,-o postulates.pdf 10.2307/1968337)";
      `P "Note various websites have DOS protections in which case \
          this may result in a 403 forbidden error. Sometimes trying \
          to use the resolver with a browser and trying again works."];
  in
  Cli_kit.cmd_with_conf "fetch" ~doc ~man @@
  let+ doi_resolvers =
    let doc =
      "$(docv) is a DOI resolver to consider. Resolution happens by \
       performing a GET on $(docv)/DOI to get a document or if a webpage \
       results, by scraping for a link to the document using various \
       heuristics. Repeatable. The $(b,https://doi.org) resolver is \
       always tested first."
    in
    let docv = "URL" in
    Arg.(value & opt_all string [] & info ["d"; "doi-resolver"] ~doc ~docv)
  and+ doi =
    let doc = "The identifier to fetch. For now only DOIs are supported." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"ID")
  and+ media_type =
    let doc = "$(docv) is media type of the document to lookup." in
    let docv = "MEDIATYPE" and pdf = "application/pdf" in
    Arg.(value & opt string pdf & info ["m"; "media-type"] ~doc ~docv)
  and+ outf =
    let doc = "Write document to $(docv). Use $(b,-) for standard output" in
    let docv = "FILE" in
    Arg.(value & opt Cli_kit.fpath Fpath.dash & info ["o"] ~doc ~docv)
  in
  fetch ~doi_resolvers ~media_type ~doi ~outf

let cmd =
  let doc = "Operations on reference documents" in
  Cli_kit.cmd_group "doc" ~doc @@
  [fetch_cmd]
