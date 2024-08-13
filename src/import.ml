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
