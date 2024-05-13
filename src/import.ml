(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type 'a entity = [ `Exists of 'a | `To_create of 'a ]

module Doi = struct
  open Result.Syntax
  open B0_json

  (* What we extract from DOI metadata *)

  type person = { family : string; given : string; orcid : string }
  type ref =
    { authors : person list;
      abstract : string;
      cites : Doi.t list;
      container_title : string;
      doi : Doi.t;
      editors : person list;
      isbn : string;
      issn : string;
      issue : string;
      issued : Crossref.partial_date;
      page : string;
      publisher : string;
      title : string;
      type' : string;
      volume : string; }

  let ref_to_short_text_citation r =
    let person p = Fmt.str "%s, %s" p.family p.given in
    let authors = function
    | [] -> "" | [p] -> person p | p :: _ -> person p ^ " et al."
    in
    Fmt.str "%s.\n%s. %d\n%s"
      r.title r.container_title (fst r.issued) (authors r.authors)

  let person_equal p0 p1 =
    if String.equal p0.orcid p1.orcid then true else
    String.equal p0.family p1.family &&
    String.equal p0.given p1.given


  (* DOI metadata query and extraction *)

  let personq =
    let person family given orcid =
      let none_is_empty v = String.trim (Option.value ~default:"" v) in
      let family = String.trim family in
      let given = none_is_empty given in
      let orcid = none_is_empty orcid in
      { family; given; orcid }
    in
    Jsonq.(succeed person $
           Crossref.Contributor.family $
           Crossref.Contributor.given $
           Crossref.Contributor.orcid)

  let refq =
    let ref
        author abstract cited_dois container_title editor isbn issn issue issued
        page publisher title type' volume doi
      =
      let none_is_empty v = String.trim (Option.value ~default:"" v) in
      let first v = match v with
      | None | Some [] -> "" | Some (t :: _) -> String.trim t
      in
      let authors = Option.value ~default:[] author in
      let abstract = Option.value ~default:"" abstract in
      let cites =
        List.map String.trim @@
        String.distinct @@
        List.filter_map Fun.id (Option.value ~default:[] cited_dois)
      in
      let container_title = first (container_title) in
      let editors = Option.value ~default:[] editor in
      let isbn = first isbn in
      let issn = first issn in
      let issue = none_is_empty issue in
      let page = none_is_empty page in
      let title = match title with [] -> "" | t :: _ -> String.trim t in
      let volume = none_is_empty volume in
      { authors; abstract; cites; container_title; doi; editors; isbn;
        issn; issue; issued; page; publisher; title; type'; volume }
    in
    Jsonq.(succeed ref $
           Crossref.Work.author personq $
           Crossref.Work.abstract $
           Crossref.Work.reference (Crossref.Reference.doi) $
           Crossref.Work.container_title $
           Crossref.Work.editor personq $
           Crossref.Work.isbn $
           Crossref.Work.issn $
           Crossref.Work.issue $
           Crossref.Work.issued $
           Crossref.Work.page $
           Crossref.Work.publisher $
           Crossref.Work.title $
           Crossref.Work.type' $
           Crossref.Work.volume)

  let get_ref httpr ~cache doi =
    Result.map_error (fun e -> Fmt.str "%s: %s" doi e) @@
    let* json = Crossref.for_doi httpr ~cache doi in
    match json with
    | None -> Ok None
    | Some json ->
        let* ref = Jsonq.query refq json in
        Ok (Some (ref doi))

  (* Converting to hyperbib entities. A bit convoluted we need to check
     which entities already exists in the db. *)

  let reference_of_ref ?(note = "") ~public ~container_id:container r =
    let isbn = if Option.is_some container then "" else r.isbn in
    Reference.v ~id:0 ~abstract:r.abstract ~container ~date:(Some r.issued)
      ~doi:r.doi ~isbn ~issue:r.issue ~note ~pages:r.page
      ~private_note:"" ~public ~publisher:r.publisher ~title:r.title
      ~type':r.type' ~volume:r.volume

  let cites_of_ref r = r.cites

  let get_container ~create_public:public db ref =
    if ref.container_title = "" then Ok None else
    let title = ref.container_title and isbn = ref.isbn and issn = ref.issn in
    let new_container () =
      Container.v
        ~id:0 ~title ~isbn ~issn ~note:"" ~private_note:"" ~public ()
    in
    let c = Container.match_stmt ~title ~isbn ~issn in
    let* cs = Db.list db c in
    match cs with
    | [c] -> Ok (Some (`Exists c))
    | [] -> Ok (Some (`To_create (new_container ())))
    | _ ->
        let has_issn c = Container.issn c = issn in
        match List.filter has_issn cs with
        | [c] -> Ok (Some (`Exists c))
        | _ ->
            let has_isbn c = Container.isbn c = isbn in
            match List.filter has_isbn cs with
            | [] -> assert false
            | [c] ->  Ok (Some (`Exists c))
            | cs ->
                Log.warn begin fun m ->
                  m "Could not disambiguate %a with %s %s %s"
                    Fmt.(list ~sep:comma int) (List.map Container.id cs)
                    title isbn issn
                end;
                Ok (Some (`Exists (List.hd cs)))

  let get_person ~create_public:public db p =
    let new_person () =
      Person.v ~id:0 ~last_name:p.family ~first_names:p.given
        ~orcid:p.orcid ~note:"" ~private_note:"" ~public ()
    in
    let m = Person.match_stmt ~last:p.family ~first:p.given ~orcid:p.orcid in
    let* ps = Db.list db m in
    match ps with
    | [p] -> Ok (`Exists p)
    | [] -> Ok (`To_create (new_person ()))
    | _ ->
        (* Try to disambiguate by orcid (even if empty) *)
        let has_orcid p' = Person.orcid p' = p.orcid in
        match List.filter has_orcid ps with
        | [] -> assert false
        | [p] -> Ok (`Exists p)
        | ps ->
            Log.warn begin fun m ->
              m "Persons %a have the same orcid, taking the first one."
                Fmt.(list ~sep:comma int) (List.map Person.id ps)
            end;
            Ok (`Exists (List.hd ps))

  let authors_editors_of_ref ~create_public db r =
    let add p acc = let* p = get_person ~create_public db p in Ok (p :: acc) in
    let* authors = List.fold_stop_on_error add r.authors [] in
    let* editors = List.fold_stop_on_error add r.editors [] in
    (* Order is important, for some people *)
    Ok (List.rev authors, List.rev editors)
end

(* This can be deleted at some point. *)

module Legacy = struct
  open Result.Syntax
  open B0_json

  module Imap = Map.Make (Int)

  let public_of ~unpublished = String.trim unpublished = ""

  type subject =
    { id : int;
      parent : string option;
      name : string;
      see : string option;
      see_also : string list;
      description : string;
      public : bool }

  let normalize_subject_name s = match String.(lowercase_ascii (trim s)) with
  | "" -> None | s -> Some s

  let subject parent subject see see_also description unpublished id =
    let parent, name = String.trim parent, String.trim subject in
    if parent = "" && name = "" then None else
    let parent, name = match parent, name with
    | p, "" -> None, p | _, _ -> normalize_subject_name parent, name
    in
    let see = normalize_subject_name see in
    let see_also =
      List.filter_map normalize_subject_name (String.split_on_char ',' see_also)
    in
    let description = String.trim description in
    let public = public_of ~unpublished in
    Some { parent; name; see; see_also; description; public; id }

  let find_unambiguous s kind nmap n = match String.Map.find_opt n nmap with
  | None | Some [] ->
      Log.warn begin fun m ->
        m "Subject '%s': %s '%s' does not exist" s.name kind n
      end;
      None
  | Some [s] -> Some s.id
  | Some ss ->
      Log.warn begin fun m ->
        m "Subject '%s': multiple %s candidates: %s"
          s.name kind (String.concat ", " (List.map (fun s -> s.name) ss))
      end;
      Some ((List.hd ss).id)

  let subjects ~file =
    let* s = Os.File.read file in
    let* json = Json.of_string ~file:(Fpath.to_string file) s in
    let subject =
      Jsonq.(succeed subject $
             mem "Subject-parent" string $
             mem "Subject" string $
             mem "See" string $
             mem "See also" string $
             mem "Description" string $
             mem "Unpublished" string)
    in
    let add s (id, nmap, imap as acc) = match s id with
    | None -> acc
    | Some s ->
        let nname = normalize_subject_name s.name |> Option.get in
        (id + 1, String.Map.add_to_list nname s nmap, Imap.add s.id s imap)
    in
    let init = 0, String.Map.empty, Imap.empty in
    let* _, nmap, imap =
      Jsonq.query Jsonq.(mem "value" (fold_array add subject init)) json
    in
    Ok (nmap, imap)

  let make_subjects nmap imap =
    let make_subject _ s acc =
      let see_also =
        let see_also that = Subject.See_also.v ~given:s.id ~that () in
        List.map see_also @@
        List.filter_map (find_unambiguous s "see also subject" nmap) s.see_also
      in
      let parent = match s.parent with
      | None -> None | Some p -> find_unambiguous s "parent" nmap p
      in
      let see = match s.see with
      | None -> None | Some see -> find_unambiguous s "see subject" nmap see
      in
      let s =
        Subject.v
          ~id:s.id ~name:s.name ~parent ~see ~description:s.description
          ~private_note:"" ~public:s.public ()
      in
      (s, see_also) :: acc
    in
    Imap.fold make_subject imap []

  let insert_subject db (s, see_also) =
    Db.string_error @@
    let* () = Db.exec db (Subject.create ~ignore_id:false s) in
    List.iter_stop_on_error
      (Db.exec db) (List.map Subject.See_also.create see_also)

  type ref =
    { doi : string;
      subjects : string list; (* lowercased *)
      note : string;
      public : bool }

  let ref doi subjects note unpublished =
    let doi = String.trim doi in
    if doi = "" then None else
    let subjects =
      List.filter_map normalize_subject_name (String.split_on_char ',' subjects)
    in
    let note = String.trim note in
    let public = public_of ~unpublished in
    Some { doi; subjects; note; public }

  let get_container_id db ref =
    match Doi.get_container ~create_public:true db ref with
    | Error _ as e -> e
    | Ok None -> Ok None
    | Ok Some (`Exists c) -> Ok (Some (Container.id c))
    | Ok Some (`To_create c) ->
        let* id = Db.insert db (Container.create ~ignore_id:true c) in
        Ok (Some id)

  let contributors db ref rid =
    let create ~role ~person ~position =
      Reference.Contributor.create @@
      Reference.Contributor.v ~reference:rid ~person ~role ~position
    in
    let rec loop role position acc = function
    | [] -> Ok acc
    | p :: ps ->
        match Doi.get_person ~create_public:true db p with
        | Error _ as e -> e
        | Ok (`Exists p) ->
            loop role (position + 1)
              (create ~role ~person:(Person.id p) ~position :: acc) ps
        | Ok (`To_create p) ->
            let* id = Db.insert db (Person.create ~ignore_id:true p) in
            loop role (position + 1)
              (create ~role ~person:id ~position :: acc) ps
    in
    let* acc = loop Person.Author 0 [] ref.Doi.authors in
    let* acc = loop Person.Editor 0 acc ref.Doi.editors in
    Ok acc

  let find_ref_subjects nmap ref =
    let find_subject s = match String.Map.find_opt s nmap with
    | None ->
        Log.warn begin fun m ->
          m "%s: unknown subject '%s'" s ref.doi
        end;
        []
    | Some ([_] as v) -> v
    | Some vs ->
        (* Disambiguate, when has parent keep only if parent is in the ref
           subjects. *)
        let filter v = match v.parent with
        | None -> Some v
        | Some p ->
            let has_p s = String.equal p s in
            if List.exists has_p ref.subjects then Some v else
            None
        in
        match List.filter_map filter vs with
        | [] ->
            let pname v = match v.parent with
            | None -> None
            | Some p -> Some (Fmt.str "'%s'" p)
            in
            Log.warn begin fun m ->
              m "Ambiguous subject '%s' in ref. %s need to add \
                 at least one of these parents: %s"
                s ref.doi (String.concat " or " (List.filter_map pname vs))
            end;
            []
        | v -> v
    in
    List.concat_map find_subject ref.subjects

  let reference_subjects_stmts nmap ref rid =
    let subjects = find_ref_subjects nmap ref in
    let add acc s =
      (Reference.Subject.create @@
       Reference.Subject.v ~reference:rid ~subject:s.id) :: acc
    in
    List.fold_left add [] subjects

  let reference_cites_stmts ref rid =
    let add acc doi =
      (Reference.Cites.create @@
       Reference.Cites.v ~reference:rid ~doi:doi) :: acc
    in
    List.fold_left add [] ref.Doi.cites

  let make_ref get_doi_ref db nmap ref =
    Log.info (fun m -> m "Importing %s" ref.doi);
    let* doi_ref = get_doi_ref ref.doi in
    match doi_ref with
    | None -> Fmt.error "%s: not found (404)" ref.doi
    | Some doi_ref ->
        Db.string_error @@
        let* container_id = get_container_id db doi_ref in
        let note = ref.note and public = ref.public in
        let r = Doi.reference_of_ref ~container_id doi_ref ~note ~public in
        let* rid = Db.insert db (Reference.create ~ignore_id:true r) in
        let subjs = reference_subjects_stmts nmap ref rid in
        let* () = List.iter_stop_on_error (Db.exec db) subjs in
        let cites = reference_cites_stmts doi_ref rid in
        let* () = List.iter_stop_on_error (Db.exec db) cites in
        let* contributors = contributors db doi_ref rid in
        let* () = List.iter_stop_on_error (Db.exec db) contributors in
        Ok ()

  let make_refs db httpr nmap refs =
    let error = Log.if_error ~use:() in
    List.iter_iter_on_error ~error (make_ref db httpr nmap) refs

  let refs ~file =
    let* s = Os.File.read file in
    let* json = Json.of_string ~file:(Fpath.to_string file) s in
    let ref = Jsonq.(succeed ref $
                     mem "DOI" string $
                     mem "Subjects" string $
                     mem "Note" string $
                     mem "Unpublished" string)
    in
    Jsonq.query Jsonq.(mem "value" (fold_array List.cons ref [])) json

  let import db conf =
    Db.with_transaction `Immediate db @@ fun db ->
    let app_dir = Hyperbib_app.Conf.app_dir conf in
    let tables_dir = Fpath.(app_dir / "tables") in
    let* nmap, imap = subjects ~file:Fpath.(tables_dir / "subjects.json") in
    let ss = make_subjects nmap imap in
    let* () = List.iter_stop_on_error (insert_subject db) ss in
    let* refs = refs ~file:Fpath.(tables_dir / "refs.json") in
    let refs = List.filter_map Fun.id refs in
    let* httpc = Result.map Option.some (Hyperbib_app.Conf.http_client conf) in
    let cache = Hyperbib_app.Conf.doi_cache_dir conf in
    let get_doi_ref = Doi.get_ref httpc ~cache in
    Ok (make_refs get_doi_ref db nmap refs)
end

let legacy = Legacy.import
