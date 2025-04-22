(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Result.Syntax
open Rel

(* FIXME we should also look into CrossRef.Ressource_link *)

let find_document httpc ~url_only ~doi_resolvers ~media_type ~doi =
  let rec loop = function
  | [] -> Fmt.error "DOI %a: No document found" (Fmt.code' Doi.pp) doi
  | resolver :: rs ->
      Log.info (fun m -> m "Trying %s" (Doi.as_url ~resolver doi));
      match Doi.to_document httpc ~url_only ~resolver ~media_type doi with
      | Error e -> Log.info (fun m -> m "%s" e); loop rs
      | Ok (url, doc) ->
          Log.info (fun m -> m "Doc found with %a" Fmt.code resolver);
          Log.info (fun m -> m "URL %a" Fmt.code url);
          Ok (resolver, url, doc)
  in
  Log.info (fun m -> m "Looking up %a" Doi.pp doi);
  loop (Doi.default_resolver :: doi_resolvers)

let doi_of_references_with_no_doc_stmt =
  let ref_dois = Row.(t2 Reference.id' Reference.doi') in
  let reference_dois =
    let open Rel_query.Syntax in
    let* r = Bag.table Reference.table in
    let has_docs =
      Bag.exists @@
      let* doc = Bag.table Reference.Doc.table in
      Bag.where
        Reference.Id.(r #. Reference.id' = doc #. Reference.Doc.reference')
        (Bag.yield doc)
    in
    let id = r #. Reference.id' in
    let doi = r #. Reference.doi' in
    Bag.where (not has_docs)
      (Bag.yield (Bag.row (fun id doi -> id, doi) $ id $ doi))
    in
    Rel_query.Sql.of_bag ref_dois reference_dois

let add_reference_doc
    ~doi ~doc ~media_type ~origin ~public ~reference db blobstore
  =
  let key, status = Blobstore.add doc blobstore |> Result.error_to_failure in
  match status with
  | Collides ->
      Log.err begin
        fun m ->
          m "@[<v>Reference %a: DOI:%a Origin:%s :@,\
             Collision on %a. Please report a bug to the software developers."
            Reference.Id.pp reference Doi.pp doi origin
            Blobstore.Key.pp key
      end
  | Exists | Created ->
      Log.stdout (fun m ->
          m "Reference %a: lookup %a"
            Reference.Id.pp reference (Fmt.code' Doi.pp) doi);
      if status = Exists
      then Log.warn (fun m -> m "%a: doc already in blob store" Doi.pp doi);
      let blob_key = Blobstore.Key.to_text key in
      let id = Reference.Doc.Id.zero in
      let doc =
        Reference.Doc.make
          ~id ~reference ~media_type ~blob_key ~name:"" ~origin ~public:false
      in
      Log.if_error ~use:() @@ Db.string_error @@ Result.join @@
      Db.with_transaction `Deferred db @@ fun db ->
      let* () = Db.exec db (Reference.Doc.create ~ignore_id:true  doc) in
      Log.stdout (fun m -> m "%a: added doc from %s" Doi.pp doi origin);
      Ok ()

let fill ~doi_resolvers ~media_type ~url_only ~public conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let* httpc = Hyperbib_conf.http_client conf in
  let* blobstore = Hyperbib_conf.blobstore conf in
  Result.join @@ Hyperbib_conf.with_db conf @@ fun db ->
  let lookup (rid, doi) () = match doi with
  | None -> Log.stdout (fun m -> m "Reference %a: no DOI." Reference.Id.pp rid);
  | Some doi ->
      match find_document httpc ~doi_resolvers ~url_only ~media_type ~doi with
      | Ok (resolver, url, doc) ->
          if url_only
          then Log.stdout (fun m -> m "%a %s" (Fmt.code' Doi.pp) doi url) else
          (* XXX Webs: the Body error handling story is missing. *)
          let doc = Http.Body.to_bytes_reader doc |> Result.get_ok' in
          let public = match public with
          | None -> String.equal resolver Doi.default_resolver
          | Some public -> public
          in
          add_reference_doc
            ~doi ~doc ~media_type ~origin:resolver ~public ~reference:rid
            db blobstore;
          Unix.sleepf 0.5; (* Throttle. *)
      | Error e -> Log.err (fun m -> m "%s" e)
  in
  let* () =
    try
      Db.fold db (Db.show_sql doi_of_references_with_no_doc_stmt) lookup ()
      |> Db.string_error
    with Failure e -> Error e
  in
  Ok Hyperbib_cli.Exit.ok

let fetch ~doi_resolvers ~media_type ~doi ~url_only ~outf conf =
  Log.if_error ~use:Hyperbib_cli.Exit.some_error @@
  let* httpc = Hyperbib_conf.http_client conf in
  (* XXX we should extract and use a resolver from doi if there is one *)
  let* doi = Doi.of_string doi in
  let* resolver, url, doc =
    find_document httpc ~url_only ~doi_resolvers ~media_type ~doi
  in
  let* () =
    if url_only then (Log.stdout (fun m -> m "%s" url); Ok ()) else
    let* doc = Http.Body.to_string doc in
    let* () = Os.File.write outf ~force:true ~make_path:true doc in
    Log.info (fun m -> m "Wrote file %a" (Fmt.code' Fpath.pp) outf);
    Ok ()
  in
  Ok Hyperbib_cli.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let doi_resolvers =
  let doc =
    "$(docv) is a DOI resolver to consider. Resolution happens by \
     performing a GET on $(docv)/DOI to get a document or if a webpage \
     results, by scraping for a link to the document using various \
     heuristics. Repeatable. The $(b,https://doi.org) resolver is \
     always tested first. If documents are resolvable via this \
     resolver they are deemed public (be careful if you are at
     your university library)."
  in
  let docv = "URL" in
  Arg.(value & opt_all string [] & info ["d"; "doi-resolver"] ~doc ~docv)

let media_type =
  let doc = "$(docv) is media type of the document to lookup." in
  let docv = "MEDIATYPE" and pdf = "application/pdf" in
  Arg.(value & opt string pdf & info ["m"; "media-type"] ~doc ~docv)

let url_only =
  let doc = "Report the URL to the document without downloading it." in
  Arg.(value & flag & info ["u"; "url-only"] ~doc)

let fill_cmd =
  let doc = "fill" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) tries to fill-in the document store for those \
          references that do not have an associated document"; ]
  in
  Hyperbib_cli.cmd_with_conf "fill" ~doc ~man @@
  let+ doi_resolvers and+ media_type and+ url_only
  and+ public =
    let doc = "Publication status of added documents" and docv = "BOOL" in
    let absent = "Only https://doi.org resolutions are public" in
    Arg.(value & opt (some bool) None & info ["public"] ~doc ~docv ~absent)
  in
  fill ~doi_resolvers ~media_type ~url_only ~public


let fetch_cmd =
  let doc = "fetch documents" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) tries to fetch a document corresponding to a \
          standard identifier and writes it on $(b,stdout).";
      `Pre "$(iname) $(b,-o postulates.pdf 10.2307/1968337)";
      `P "Note various websites have DOS protections in which case \
          this may result in a 403 forbidden error. Sometimes trying \
          to use the resolver with a browser and trying again works."];
  in
  Hyperbib_cli.cmd_with_conf "fetch" ~doc ~man @@
  let+ doi_resolvers and+ media_type and+ url_only
  and+ doi =
    let doc =
      "The identifier to fetch. For now only DOIs are supported or the \
       identifier of a reference in the database are supported."
    in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"ID")
  and+ outf =
    let doc = "Write document to $(docv). Use $(b,-) for standard output" in
    let docv = "FILE" in
    Arg.(value & opt Hyperbib_cli.fpath Fpath.dash & info ["o"] ~doc ~docv)
  in
  fetch ~doi_resolvers ~media_type ~doi ~url_only ~outf

let cmd =
  let doc = "Operations on reference documents" in
  Hyperbib_cli.cmd_group "doc" ~doc @@
  [fetch_cmd; fill_cmd]
