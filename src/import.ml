(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type 'a entity = [ `Exists of 'a | `To_create of 'a ]

module Doi = struct
  open Result.Syntax

  type person = Crossref.Contributor.t
  type ref = Crossref.Work.t

  let first = function [] -> "" | t :: _ -> String.trim t
  let none_is_empty v = String.trim (Option.value ~default:"" v)

  let title (r : ref) = first r.title
  let container_title (r : ref) = first r.container_title
  let cited_dois (r : ref) =
    let doi r = r.Crossref.Reference.doi in
    List.sort_uniq Doi.compare @@ List.filter_map doi r.reference

  let ref_to_short_text_citation (r : ref) =
    let person (p : person) = Fmt.str "%s, %s" p.family p.given in
    let authors = function
    | [] -> "" | [p] -> person p | p :: _ -> person p ^ " et al."
    in
    Fmt.str "%s.\n%s. %d\n%s"
      (title r) (container_title r) (fst r.issued) (authors r.author)

  let get_ref httpr ~cache doi =
    Result.map_error (fun e -> Fmt.str "%a: %s" Doi.pp doi e) @@
    Crossref.for_doi httpr ~cache doi

  (* Converting to hyperbib entities. A bit convoluted we need to check
     which entities already exists in the db. *)

  let reference_of_ref ?(note = "") ~public ~container_id:container (r : ref) =
    let isbn = if Option.is_some container then "" else (first r.isbn) in
    Reference.make ~id:Reference.Id.zero ~abstract:r.abstract ~container
      ~created_ptime_s:(Unix.gettimeofday ())
      ~date:(Some r.issued) ~doi:(Some r.doi) ~isbn
      ~issue:(none_is_empty r.issue) ~note ~pages:(none_is_empty r.page)
      ~private_note:"" ~public ~publisher:r.publisher ~title:(title r)
      ~type':r.type' ~volume:(none_is_empty r.volume)


  let cites_of_ref (r : ref) = cited_dois r

  let get_container ~create_public:public db ref =
    let container_title = container_title ref in
    if container_title = "" then Ok None else
    let title = container_title in
    let isbn = first ref.isbn and issn = first ref.issn in
    let id = Container.Id.zero in
    let new_container () =
      Container.make ~id ~title ~isbn ~issn ~note:"" ~private_note:"" ~public ()
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
                    Fmt.(list ~sep:comma Container.Id.pp)
                    (List.map Container.id cs)
                    title isbn issn
                end;
                Ok (Some (`Exists (List.hd cs)))

  let get_person ~create_public:public db (p : Crossref.Contributor.t) =
    let new_person () =
      Person.make ~id:Person.Id.zero ~last_name:p.family ~first_names:p.given
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
                Fmt.(list ~sep:comma Person.Id.pp) (List.map Person.id ps)
            end;
            Ok (`Exists (List.hd ps))

  let authors_editors_of_ref ~create_public db (r : ref) =
    let add p acc = let* p = get_person ~create_public db p in Ok (p :: acc) in
    let* authors = List.fold_stop_on_error add r.author [] in
    let* editors = List.fold_stop_on_error add r.editor [] in
    (* Order is important, for some people *)
    Ok (List.rev authors, List.rev editors)
end
