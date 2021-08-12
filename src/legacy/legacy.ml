(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

module Page_url = struct
  type t =
  | Home
  | Help
  | Refs
  | Subjects
  | Persons
  | Years
  | Containers

  let dec_page u v = let* `GET = Kurl.Allow.(meths [get] u) in Kurl.ok v
  let dec u = match Kurl.Bare.path u with
  | ["index"] -> dec_page u Home
  | ["help"] -> dec_page u Help
  | ["references"] -> dec_page u Refs
  | ["subjects"] -> dec_page u Subjects
  | ["persons"] -> dec_page u Persons
  | ["containers"] -> dec_page u Containers
  | ["years"] -> dec_page u Years
  | _ -> Kurl.no_match

  let enc_get_page p = Kurl.Bare.v `GET p ~ext:".html"
  let enc = function
  | Home -> enc_get_page ["index"]
  | Help -> enc_get_page ["help"]
  | Refs -> enc_get_page ["references"]
  | Subjects -> enc_get_page ["subjects"]
  | Persons -> enc_get_page ["persons"]
  | Years -> enc_get_page ["years"]
  | Containers -> enc_get_page ["containers"]

  let kind = Kurl.kind enc dec
end

module Person' = struct
  let filter sat s = (* FIXME move that somewhere *)
    let max_idx = String.length s - 1 in
    let rec with_buf b k i = (* k is the write index in b *)
      if i > max_idx then Bytes.sub_string b 0 k else
      let c = String.unsafe_get s i in
      if sat c then (Bytes.unsafe_set b k c; with_buf b (k + 1) (i + 1)) else
      with_buf b k (i + 1)
    in
    let rec try_no_alloc i =
      if i > max_idx then s else
      if (sat (String.unsafe_get s i)) then try_no_alloc (i + 1) else
      if i = max_idx then String.sub s 0 i else
      let b = Bytes.of_string s in (* copy and overwrite starting from i *)
      with_buf b i (i + 1)
    in
    try_no_alloc 0

  type t =
    { uid : int;
      family : string;
      family_term : string;
      given : string option;
      given_term : string;
      orcid : string option; }

  let cleanup = function
  | None -> None
  | Some s -> let s = String.trim s in if s = "" then None else Some s

  let uid = ref (-1)

  let cleanup = function ',' | '.' | '\'' | '(' | ')' -> false | _ -> true
  let family_term f =
    let f = String.lowercase_ascii (filter cleanup f) in
    match String.cut_left ~sep:"de " f with
    | Some (_, n) -> n
    | None -> f

  let given_term = function
  | None -> ""
  | Some g ->
      let g = String.map (function '.' | ',' -> ' ' | c -> c) g in
      let ns = String.split_on_char ' ' g in
      let b = Buffer.create 10 in
      let add_initial s = if s <> "" then Buffer.add_char b s.[0] else () in
      List.iter add_initial ns; Buffer.contents b

  let v ~family ~given ~orcid () =
    let family_term = family_term family in
    let given_term = given_term given in
    incr uid; { uid = !uid; family; family_term; given; given_term; orcid }

  let uid p = p.uid
  let family p = p.family
  let family_term p = p.family_term
  let given p = p.given
  let given_term p = p.given_term
  let orcid p = p.orcid
  let anonymous = v ~family:"Anonymous" ~given:None ~orcid:None ()

  let term p = p.family_term ^ p.given_term
  let compare p0 p1 = (compare : int -> int -> int) p0.uid p1.uid
  let migrate p =
    let given = match p.given with None -> "" | Some g -> g in
    let orcid = match p.orcid with None -> "" | Some id -> id in
    Person.v
      ~id:p.uid ~last_name:p.family ~first_names:given ~orcid
      ~note:"" ~private_note:"" ~public:true ()
end

module Subject' = struct
  type t =
    { uid : int;
      name : string;
      parent : string option;
      see : string option;
      see_also : string list;
      description : string option }

  let cleanup = function
  | None -> None
  | Some s -> let s = String.trim s in if s = "" then None else Some s

  let uid = ref (-1)

  let v ~name ~parent ~see ~see_also ~description =
    let parent = cleanup parent and description = cleanup description in
    incr uid; { uid = !uid; name; parent; see; see_also; description }

  let uid r = r.uid
  let name s = s.name
  let parent s = s.parent
  let see s = s.see
  let see_also s = s.see_also
  let description s = s.description
  let compare_by_name s0 s1 = String.compare s0.name s1.name

  module T = struct
    type nonrec t = t
    let compare s0 s1 = (compare : int -> int -> int) s0.uid s1.uid
  end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  module Db = struct
    type subject = t
    type t =
      { subjects : subject list;
        by_name : subject list String.Map.t;
        parent : subject Map.t;
        children : subject list Map.t; }

    let empty =
      { subjects = []; by_name = String.Map.empty; parent = Map.empty;
        children = Map.empty }

    let add_by_name s by_name = String.Map.add_to_list s.name by_name
    let add db s =
      let subjects = s :: db.subjects in
      let by_name = String.Map.add_to_list s.name s db.by_name in
      { db with subjects; by_name }

    let make_hierarchy db =
      let rec loop ps children = function
      | [] -> ps, children
      | s :: ss ->
          match parent s with
          | None -> loop ps children ss
          | Some p ->
              match String.Map.find_opt p db.by_name with
              | Some [p] ->
                  let ps = Map.add s p ps in
                  let cs = match Map.find_opt p children with
                  | None -> [s] | Some cs -> List.sort compare_by_name (s :: cs)
                  in
                  loop ps (Map.add p cs children) ss
              | None ->
                  Log.warn begin fun m ->
                    m "Subject '%s': parent '%s' does not exist" s.name p
                  end;
                  loop ps children ss
              | Some cs ->
                  Log.warn begin fun m ->
                    m "Subject '%s': multiple parent candidates: %s"
                      s.name (String.concat ", " (List.map name cs))
                  end;
                  loop ps children ss
      in
      let parent, children = loop Map.empty Map.empty db.subjects in
      let subjects = List.sort compare_by_name db.subjects in
      { db with subjects; parent; children }

    let find n db = String.Map.find_opt n db.by_name
    let find_parent s db = Map.find_opt s db.parent
    let find_children s db = match Map.find_opt s db.children with
    | None -> [] | Some cs -> cs

    let find_see s db = match s.see with
    | None -> None
    | Some see ->
        match String.Map.find_opt see db.by_name with
        | None ->
            Log.warn begin fun m ->
              m "Subject '%s': see '%s' undefined" (name s) see
            end;
            None
        | Some [v] -> Some v
        | Some vs ->
            match find_parent s db with
            | None ->
                Log.warn begin fun m ->
                  m "Subject '%s': ambiguous see '%s' and no parent found"
                    (name s) see
                end;
                None
            | Some p ->
                let with_p v = match find_parent v db with
                | Some p' when (uid p' = uid p) -> true
                | None | Some _ -> false
                in
                List.find_opt with_p vs


    let find_see_also s db =
      let add acc a = match String.Map.find_opt a db.by_name with
      | None ->
          Log.warn begin fun m ->
            m "Subject '%s': see also '%s' undefined" (name s) a
          end;
          acc
      | Some [s] -> s :: acc
      | Some cs ->
          Log.warn begin fun m ->
            m "Subject '%s': see also '%s' has multiple candidates"
              (name s) a
          end;
          acc
      in
      List.sort compare_by_name (List.fold_left add [] s.see_also)

    let roots db =
      let add acc s = if Map.mem s db.parent then acc else s :: acc in
      List.fold_left add [] db.subjects

    let all db = db.subjects
  end

  let migrate sdb s =
    let description = match description s with None -> "" | Some d -> d in
    let parent = Option.map uid (Db.find_parent s sdb) in
    let see = Option.map uid (Db.find_see s sdb) in
    let see_also =
      let sas = List.map uid (Db.find_see_also s sdb) in
      List.map (fun that -> Subject.See_also.v ~given:(uid s) ~that ()) sas
    in
    let s =
      Subject.v ~id:(uid s) ~name:(name s) ~parent ~see ~description
        ~private_note:"" ~public:true ()
    in
    s, see_also
end

module Container' = struct
  type t = string
  let migrate c =
    Container.v
      ~id:0 ~title:c ~isbn:"" ~issn:"" ~note:"" ~private_note:"" ~public:true ()
end

module Reference' = struct
  type partial_date = int * (int * int option) option
  type t =
    { uid : int;
      authors : Person'.t list;
      bibtex_src : Bibtex.t;
      cites : Doi.t list;
      container_title : string option;
      date : partial_date option;
      doi : Doi.t option;
      editors : Person'.t list;
      issue : string option;
      type' : string;
      note : string option;
      pages : string option;
      publisher : string option;
      subjects : string list;
      title : string option;
      volume : string option; }

  let uid = ref (-1)
  let v
      ~authors ~bibtex_src ~cites ~container_title ~date ~doi ~editors ~issue
      ~type' ~note ~pages ~publisher ~subjects ~title ~volume
    = incr uid;
    { uid = !uid; authors; bibtex_src; cites; container_title; date; doi;
      editors; issue; type'; note; pages; publisher; subjects; title; volume; }

  let uid r = r.uid
  let authors r = r.authors
  let bibtex_src r = r.bibtex_src
  let container_title r = r.container_title
  let date r = r.date
  let doi r = r.doi
  let editors r = r.editors
  let issue r = r.issue
  let type' r = r.type'
  let note r = r.note
  let pages r = r.pages
  let publisher r = r.publisher
  let cites r = r.cites
  let subjects r = r.subjects
  let title r = r.title
  let volume r = r.volume

  (* Derived  *)

  let year r = match r.date with None -> 0 | Some (y, _) -> y
  let compare_by_date r0 r1 = compare r0.date r1.date

  module T = struct
    type nonrec t = t
    let compare r0 r1 = (compare : int -> int -> int) r0.uid r1.uid
  end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let find_container_id_by_title cs = function
  | None -> None
  | Some t ->
      let eq_title c = String.equal (Container.title c) t in
      Option.map Container.id (List.find_opt eq_title cs)

  let migrate cs r =
    let none_is_empty = function None -> "" | Some s -> s in
    Reference.v ~id:r.uid
      ~abstract:""
      ~container:(find_container_id_by_title cs r.container_title)
      ~date:r.date
      ~doi:r.doi
      ~isbn:""
      ~issue:(none_is_empty r.issue)
      ~note:(none_is_empty r.note)
      ~pages:(none_is_empty r.pages)
      ~private_note:""
      ~public:true
      ~publisher:(none_is_empty r.publisher)
      ~title:(none_is_empty r.title)
      ~type':r.type'
      ~volume:(none_is_empty r.volume)

  type migrate_person_map = Person.t B00_std.String.Map.t
  type migrate_subject_map = Subject.t B00_std.String.Map.t

  let migrate_person_key p = Person.last_name p ^ Person.first_names p
  let migrate_person'_key p =
    Person'.family p ^ (Option.value ~default:"" (Person'.given p))

  let migrate_person_map ps =
    let add acc p = String.Map.add (migrate_person_key p) p acc in
    List.fold_left add String.Map.empty ps

  let migrate_subject_map ss =
    let add acc s = String.Map.add (Subject.name s) s acc in
    List.fold_left add String.Map.empty ss

  let migrate_contributors pmap r =
    let add role (position, acc) p =
      let acc = match String.Map.find_opt (migrate_person'_key p) pmap with
      | Some p ->
          (Reference.Contributor.v
            ~reference:(uid r) ~person:(Person.id p) ~role ~position)
          :: acc
      | None ->
          Log.warn (fun m -> m "Person not found: '%s' '%s'"
                       (Person'.family p)
                       (Option.value ~default:"" (Person'.given p)));
          acc
      in
      position + 1, acc
    in
    let add_author = add Person.Author in
    let add_editor = add Person.Editor in
    let _, cs = List.fold_left add_author (0, []) (authors r) in
    let _, cs = List.fold_left add_editor (0, cs) (editors r) in
    cs

  let migrate_subjects smap r =
    let add acc s = match String.Map.find_opt s smap with
    | None -> Log.warn (fun m -> m "Subject not found: %s'" s); acc
    | Some s ->
        Reference.Subject.v ~reference:(uid r) ~subject:(Subject.id s) :: acc
    in
    List.fold_left add [] (subjects r)

  let migrate_cites r =
    let migrate doi = Reference.Cites.v ~reference:(uid r) ~doi in
    let cites = String.uniquify (cites r) in
    List.map migrate cites
end

module Refdb = struct
  type error = [ `Unknown_subjects of Reference'.t * string list ]

  module Int_map = Map.Make (Int)
  module Person_idx = struct
    type t = Person'.t
    let compare p0 p1 =
      let c = Person'.compare p0 p1 in
      if c = 0 then 0 else
      match Person'.orcid p0, Person'.orcid p1 with
      | Some id0, Some id1 when String.equal id0 id1 -> 0
      | _, _ ->
          match Person'.family p0, Person'.family p1 with
          | f0, f1 when not (String.equal f0 f1) -> String.compare f0 f1
          | _, _ ->
              match Person'.given p0, Person'.given p1 with
              | g0, g1 when g0 = g1 -> 0
              | _, _ -> c
  end

  module Person_set = Set.Make (Person_idx)
  module Person_map = struct
    include Map.Make (Person_idx)
    let add_to_list k v m = match find_opt k m with
    | None -> add k [v] m | Some l -> add k (v :: l) m
  end

  type t =
    { subjects : Subject'.Db.t;
      refs : Reference'.t list;
      by_doi : Reference'.t String.Map.t;
      by_container_title : Reference'.t list String.Map.t;
      by_person : Reference'.t list Person_map.t;
      by_subject : Reference'.t list Subject'.Map.t;
      by_year : Reference'.t list Int_map.t }

  let empty subjects =
    { subjects = subjects;
      refs = [];
      by_doi = String.Map.empty;
      by_container_title = String.Map.empty;
      by_person = Person_map.empty;
      by_subject = Subject'.Map.empty;
      by_year = Int_map.empty; }

  let subjects db = db.subjects

  let add_ref_doi r by_doi = match Reference'.doi r with
  | None -> by_doi | Some doi -> String.Map.add doi r by_doi

  let add_ref_container_title r by_container_title =
    match Reference'.container_title r with
    | None -> by_container_title
    | Some t -> String.Map.add_to_list t r by_container_title

  let add_ref_persons r by_person =
    let add acc p = Person_map.add_to_list p r acc in
    let by_person = List.fold_left add by_person (Reference'.authors r) in
    List.fold_left add by_person (Reference'.editors r)

  let add_ref_subjects subjects r by_subject =
    let add acc subject = match Subject'.Map.find_opt subject acc with
    | None -> Subject'.Map.add subject [r] acc
    | Some rs -> Subject'.Map.add subject (r :: rs) acc
    in
    let r_subjects =
      let r_subjects = Reference'.subjects r in
      let subject k =
        let doi r = Option.fold ~none:"" ~some:Fun.id (Reference'.doi r) in
        match Subject'.Db.find k subjects with
        | None ->
            Log.warn begin fun m ->
              m "Subject '%s': undefined in ref %s" k (doi r)
            end;
            []
        | Some ([_] as v) -> v
        | Some vs ->
            (* Disambiguate keep only if parent is in the ref subjects. *)
            let filter v = match Subject'.Db.find_parent v subjects with
            | None -> Some v
            | Some p ->
                let has_p s = String.equal (Subject'.name p) s in
                if List.exists has_p r_subjects then Some v else
                None
            in
            match List.filter_map filter vs with
            | [] ->
                let pname v =
                  match Subject'.Db.find_parent v subjects with
                  | None -> None
                  | Some p -> Some (Fmt.str "'%s'" (Subject'.name p))
                in
                Log.warn begin fun m ->
                  m "Ambiguous subject '%s' in ref. %s need to add \
                     at least one of these parents: %s"
                    k (doi r) (String.concat " or " (List.filter_map pname vs))
                end;
                []
            | v -> v
      in
      List.concat_map subject r_subjects
    in
    List.fold_left add by_subject r_subjects

  let add_ref_year r by_year =
    let y = Reference'.year r in
    match Int_map.find_opt y by_year with
    | None -> Int_map.add y [r] by_year
    | Some refs -> Int_map.add y (r :: refs) by_year

  let add db r =
    let by_doi = add_ref_doi r db.by_doi in
    let by_container_title = add_ref_container_title r db.by_container_title in
    let by_person = add_ref_persons r db.by_person in
    let by_subject = add_ref_subjects db.subjects r db.by_subject in
    let by_year = add_ref_year r db.by_year in
    let db = { db with refs = r :: db.refs; by_doi; by_container_title;
                       by_person; by_subject; by_year }
    in
    db

  let refs db = db.refs
  let keys k _ acc = k :: acc

  let citations db r =
    let dois = Reference'.cites r in
    List.filter_map (fun d -> String.Map.find_opt d db.by_doi) dois

  let container_title_index db = String.Map.fold keys db.by_container_title []
  let find_container_title db t =
    Option.value ~default:[] (String.Map.find_opt t db.by_container_title)

  let subject_index db = Subject'.Map.fold keys db.by_subject []
  let find_subject db k = match Subject'.Map.find_opt k db.by_subject with
  | None -> [] | Some l -> l

  let person_index db =
    let compare p0 p1 =
      let c =
        String.compare (Person'.family_term p0) (Person'.family_term p1)
      in
      if c <> 0 then c else
      compare (Person'.given_term p0) (Person'.given_term p1)
    in
    List.sort compare @@ Person_map.fold keys db.by_person []

  let find_person db p = match Person_map.find_opt p db.by_person with
  | None -> []
  | Some l -> Reference'.Set.elements (Reference'.Set.of_list l)

  let year_index db = Int_map.fold keys db.by_year []
  let find_year db y = match Int_map.find_opt y db.by_year with
  | None -> [] | Some l -> l
end

module Data_tables = struct
  open Result.Syntax
  open B00_serialk_json

  let entry doi subjects note unpublished =
    if String.trim unpublished <> "" then None else
    let add_non_blank k v m =
      let v = String.trim v in
      if v = "" then m else String.Map.add k v m
    in
    let fields =
      String.Map.empty
      |> add_non_blank "doi" doi
      |> add_non_blank "annote" note
      |> add_non_blank "keywords" subjects
    in
    Some (Bibtex.v ~type':"unknown"
            ~cite_key:doi (* likely illegal bibtex *) ~fields ())

  let refs_to_bibs ~file =
    let* s = Os.File.read file in
    let* json = Json.of_string ~file:(Fpath.to_string file) s in
    let entry = Jsonq.(succeed entry $
                       mem "DOI" string $
                       mem "Subjects" string $
                       mem "Note" string $
                       mem "Unpublished" string)
    in
    let add_some v acc = match v with
    | None -> acc | Some v -> v :: acc
    in
    Jsonq.query Jsonq.(mem "value" (fold_array add_some entry [])) json

  let subject parent subject see see_also description unpublished =
    let parent, name = String.trim parent, String.trim subject in
    if (parent = "" && name = "") || String.trim unpublished <> ""
    then None else
    let parent, name = match parent, name with
    | p, "" -> None, p | _, _ -> Some parent, name
    in
    let see = match String.trim see with "" -> None | s -> Some s in
    let see_also =
      List.filter (Fun.negate (String.equal "")) @@
      List.map String.trim (String.split_on_char ',' see_also)
    in
    let description = Some description in
    Some (Subject'.v ~name ~parent ~see ~see_also ~description)

  let subjs_to_subjects ~file =
    let* s = Os.File.read file in
    let* json = Json.of_string ~file:(Fpath.to_string file) s in
    let subject = Jsonq.(succeed subject  $
                         mem "Subject-parent" string $
                         mem "Subject" string $
                         mem "See" string $
                         mem "See also" string $
                         mem "Description" string $
                         mem "Unpublished" string)
    in
    let add_some v acc = match v with
    | None -> acc | Some v -> v :: acc
    in
    Jsonq.query Jsonq.(mem "value" (fold_array add_some subject [])) json
end


(* Munge *)

let ref_of_bib_and_doi_meta doi bib json =
  let open B00_serialk_json in
  let ref
      authors container_title doi date editors issue type' publisher pages
      refs subjects title volume bib
    =
    let authors = Option.value ~default:[] authors in
    let editors = Option.value ~default:[] editors in
    let container_title = match container_title with
    | None | Some [] -> None | Some (t :: _) -> Some t
    in
    let date = Some date in
    let publisher = Some publisher in
    let cites = List.filter_map Fun.id (Option.value ~default:[] refs) in
    let subjects =
      List.sort compare @@
      Option.value ~default:[] (Bibtex.keywords bib)
    in
    let note = Bibtex.annote bib in
    let title = match title with [] -> None | t :: _ -> Some t in
    Reference'.v
      ~authors ~bibtex_src:bib ~container_title ~date ~doi ~editors ~issue
      ~type' ~note ~publisher ~pages ~cites ~subjects ~title ~volume
  in
  let personq =
    let person family given orcid = Person'.v ~family ~given ~orcid () in
    Jsonq.(succeed person $
           Crossref.Contributor.family $ Crossref.Contributor.given $
           Crossref.Contributor.orcid)
  in
  Jsonq.query
    Jsonq.(succeed ref $
           Crossref.Work.author personq $ Crossref.Work.container_title $
           succeed (Some doi) $ Crossref.Work.issued $
           Crossref.Work.editor personq $ Crossref.Work.issue $
           Crossref.Work.type' $
           Crossref.Work.publisher $ Crossref.Work.page $
           Crossref.Work.reference (Crossref.Reference.doi) $
           Crossref.Work.subject $ Crossref.Work.title $ Crossref.Work.volume $
           succeed bib)
    json

let get_ref httpr dois_dir ~src bib = match Bibtex.doi bib with
| None -> None
| Some doi ->
    (*    Result.map_error (fun err -> Some doi, err) @@ *)
    Log.if_error ~use:None @@
    Result.map_error (fun e -> Fmt.str "%s: %s" doi e) @@
    let* json = Crossref.for_doi httpr ~cache:dois_dir doi in
    let* ref = ref_of_bib_and_doi_meta doi bib json in
    Ok (Some ref)

let refdb_of_tables app_dir =
  let tables_dir = Fpath.(app_dir / "tables") in
  let dois_dir = Fpath.(app_dir / "dois") in
  let refs_table = Fpath.(tables_dir / "refs.json") in
  let subjs_table = Fpath.(tables_dir / "subjects.json") in
  let* bibs = Data_tables.refs_to_bibs ~file:refs_table in
  let* httpr = Result.map Option.some (B00_http.Httpr.get_curl ()) in
  let refs = List.filter_map (get_ref httpr dois_dir ~src:Fpath.dash) bibs in
  let* subjs = Data_tables.subjs_to_subjects ~file:subjs_table in
  let subj_db =
    let db =
      List.fold_left Subject'.Db.add Subject'.Db.empty subjs
    in
    Subject'.Db.make_hierarchy db
  in
  Ok (List.fold_left Refdb.add (Refdb.empty subj_db) refs)

(* Import *)

let db_exec db st =
  match (Db.error_string @@ Db.exec db st) with
  | Error e -> Log.err (fun m -> m "@[<v>%s@,%a@]" e Ask.Sql.Stmt.pp st)
  | Ok () -> ()

let import_containers refdb db =
  let cs = Refdb.container_title_index refdb in
  let cs = List.map Container'.migrate cs in
  let stmts = List.map (Container.create ~ignore_id:true) cs in
  List.iter (db_exec db) stmts;
  Db.list db (Container.list_stmt ~only_public:false)

let all_persons refdb = (* Refdb.person_index does some norm. bizess. *)
  let add_person acc p = Refdb.Person_set.add p acc in
  let add_persons acc r =
    let acc = List.fold_left add_person acc (Reference'.authors r) in
    List.fold_left add_person acc (Reference'.editors r)
  in
  List.fold_left add_persons Refdb.Person_set.empty (Refdb.refs refdb)

let import_persons refdb db =
  let ps = Refdb.Person_set.elements (all_persons refdb) in
  let ps = List.map Person'.migrate ps in
  let stmts = List.map (Person.create ~ignore_id:false) ps in
  List.iter (db_exec db) stmts;
  Db.list db (Person.list_stmt ~only_public:false)

let import_subjects refdb db =
  let sdb = Refdb.subjects refdb in
  let ss = Subject'.Db.all sdb in
  let ss = List.map (Subject'.migrate sdb) ss in
  let stmt (s, see_also) =
    Subject.create ~ignore_id:false s ::
    List.map Subject.See_also.create see_also
  in
  let stmts = List.concat_map stmt ss in
  List.iter (db_exec db) stmts;
  Db.list db (Subject.list_stmt ~only_public:false)

let import_references refdb db cs ps ss =
  let pmap = Reference'.migrate_person_map ps in
  let smap = Reference'.migrate_subject_map ss in
  let rs = Refdb.refs refdb in
  let rs' = List.map (Reference'.migrate cs) rs in
  let stmts = List.map (Reference.create ~ignore_id:false) rs' in
  List.iter (db_exec db) stmts;
  let cites = List.concat_map Reference'.migrate_cites rs in
  let stmts = List.map Reference.Cites.create cites in
  List.iter (db_exec db) stmts;
  let cs = List.concat_map (Reference'.migrate_contributors pmap) rs in
  let stmts = List.map Reference.Contributor.create cs in
  List.iter (db_exec db) stmts;
  let subjects = List.concat_map (Reference'.migrate_subjects smap) rs in
  let stmts = List.map Reference.Subject.create subjects in
  List.iter (db_exec db) stmts

let import data_conf =
  Log.if_error ~use:Hyperbib.Exit.some_error @@
  let* refdb = refdb_of_tables (Hyperbib.Data_conf.app_dir data_conf) in
  let* () = Hyperbib.Data_conf.ensure_data_dir data_conf in
  let db_file = Hyperbib.Data_conf.db_file data_conf in
  Result.map_error (fun e -> Fmt.str "%a: %s" Fpath.pp_unquoted db_file e) @@
  Db.error_string @@
  let foreign_keys = false in
  let* db = Db.open' ~foreign_keys (Hyperbib.Data_conf.db_file data_conf) in
  let* () = Db.setup ~schema:Schema.tables ~drop_if_exists:true db in
  let* () = Result.join @@ Db.with_transaction `Immediate db @@ fun db ->
    let* cs = import_containers refdb db in
    let* ps = import_persons refdb db in
    let* ss = import_subjects refdb db in
    import_references refdb db cs ps ss;
    Ok ()
  in
  Ok Hyperbib.Exit.ok

(* Tools *)

module Gen_csv = struct
  let escape_dquotes s =
    let char_len = function '"' -> 2 | _ -> 1 in
    let set_char b i = function
    | '"' -> Bytes.set b i '\"'; Bytes.set b (i+1) '"'; i + 2
    | c -> Bytes.set b i c; i + 1
    in
    String.byte_escaper char_len set_char s

  let field s = String.concat "" ["\""; escape_dquotes s; "\""]

  let person p = Person'.family p
  let authors = function
  | [] -> "Unknown"
  | ps -> String.concat ", " (List.map person ps)

  let col_names =
    String.concat ","
      [field "DOI";
       field "Authors"; field "Title"; field "Subjects"; field "Note" ]

  let ref r =
    let fields = [
      Option.value (Reference'.doi r) ~default:"";
      authors (Reference'.authors r);
      Option.value (Reference'.title r) ~default:"";
      String.concat ", " (Reference'.subjects r);
      Option.value (Bibtex.annote (Reference'.bibtex_src r)) ~default:"";
    ]
    in
    String.concat "," (List.map field fields)

  let gen refs = Ok (String.concat "\n" (col_names :: (List.map ref refs)))
end

let gen_csv = Gen_csv.gen


(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
