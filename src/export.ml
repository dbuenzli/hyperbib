(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

module Static_html = struct
  open Result.Syntax

  (* N.B. Some of the data gathering here is similar to the one done in
     services. Needs streamlining.

     Also if this uses too much memory/is too slow, it's because we are
     repeatedly reading the same data from the db. But let's go with this
     for now.  *)

  let refs_render_data ~only_public refs db =
    Reference.render_data ~only_public refs db |> Db.string_error

  let filepath_of_url uf u =
    let uf = Kurl.Fmt.with_fmt ~use_exts:true uf in
    let req = Kurl.Fmt.bare uf u in
    let* path = Http.Path.to_absolute_filepath (Kurl.Bare.path req) in
    let file = String.subrange ~first:1 (* drop / *) path ^ Kurl.Bare.ext req in
    Result.map_error (fun e -> Fmt.str "URL %s to file path: %s" path e) @@
    Fpath.of_string file

  let write_page ~dir g p =
    let url = Page.url p in
    let html = El.to_string ~doctype:true (Page.html p) in
    let* file = filepath_of_url (Page.Gen.url_fmt g) url in
    let file = Fpath.(dir // file) in
    Os.File.write ~force:true ~make_path:true file html

  let write_reference ~dir db g r =
    let only_public = Rel_query.Bool.true' in
    let rid = Reference.Id.v (Reference.id r) in
    let ref = Reference.find_id rid in
    let* render_data =
      Reference.render_data ~only_public ref db |> Db.string_error
    in
    let cites = Reference.find_dois (Reference.dois_cited rid) in
    let* cites = refs_render_data ~only_public cites db in
    let cited_by = match Reference.doi r with
    | None -> Bag.empty
    | Some doi -> Reference.citing_doi (Rel_query.Text.v doi)
    in
    let* cited_by = refs_render_data ~only_public cited_by db in
    write_page ~dir g (Reference_html.page g r ~render_data ~cites ~cited_by)

  let write_references ~dir db g =
    let only_public = Rel_query.Bool.true' in
    let all = Reference.list ~only_public in
    let* rs = refs_render_data ~only_public all db in
    let index = Reference_html.index g rs in
    let* () = write_page ~dir g index in
    let write_reference = write_reference ~dir db g in
    let* () = List.iter_stop_on_error write_reference rs.Reference.list in
    Ok ()

  let write_container ~dir db g c =
    let only_public = Rel_query.Bool.true' in
    let all = Reference.list ~only_public in
    let id = Container.Id.v (Container.id c) in
    let refs = Reference.filter_container_id id all in
    let* refs = refs_render_data ~only_public refs db in
    write_page ~dir g (Container_html.page g c refs)

  let write_containers ~dir db g =
    let containers = Container.list_stmt ~only_public:true in
    let* cs = Db.list db containers |> Db.string_error in
    let ref_count = Reference.container_public_ref_count_stmt in
    let* ref_count = Container.id_map db ref_count fst |> Db.string_error in
    let index = Container_html.index g cs ~ref_count in
    let* () = write_page ~dir g index in
    let write_container = write_container ~dir db g in
    let* () = List.iter_stop_on_error write_container cs in
    Ok ()

  let write_person ~dir db g p =
    let only_public = Rel_query.Bool.true' in
    let all = Reference.list ~only_public in
    let id = Person.Id.v (Person.id p) in
    let refs = Reference.filter_person_id id all in
    let* refs = refs_render_data ~only_public refs db  in
    write_page ~dir g (Person_html.page g p refs)

  let write_persons ~dir db g =
    let persons = Person.list_stmt ~only_public:true in
    let* ps = Db.list db persons |> Db.string_error in
    let ref_count = Reference.persons_public_ref_count_stmt in
    let* ref_count = Person.id_map db ref_count fst |> Db.string_error in
    let index = Person_html.index g ps ~ref_count in
    let* () = write_page ~dir g index in
    let write_person = write_person ~dir db g in
    let* () = List.iter_stop_on_error write_person ps in
    Ok ()

  let write_subject ~dir db g s =
    let only_public = Rel_query.Bool.true' in
    let all = Reference.list ~only_public in
    let id = Subject.Id.v (Subject.id s) in
    let refs = Reference.Subject.filter_subject_id id all in
    let* parent = match Subject.parent s with
    | None -> Ok None
    | Some pid -> Db.first db (Subject.find_id_stmt pid) |> Db.string_error
    in
    let* refs = refs_render_data ~only_public refs db in
    write_page ~dir g (Subject_html.page g s ~parent refs)

  let write_subjects ~dir db g =
    let subjects = Rel_query.Sql.of_bag' Subject.table Subject.visible_list in
    let* ss = Db.list db subjects |> Db.string_error in
    let ref_count = Reference.subject_public_ref_count_stmt in
    let* ref_count = Subject.id_map db ref_count fst |> Db.string_error in
    let index = Subject_html.index g ss ~ref_count in
    let* () = write_page ~dir g index in
    let write_subject = write_subject ~dir db g in
    let* () = List.iter_stop_on_error write_subject ss in
    Ok ()

  let write_year ~dir db g (year, _) =
    let only_public = Rel_query.Bool.true' in
    let all = Reference.list ~only_public in
    let refs = Year.filter ~year:(Rel_query.Int.v year) all in
    let* render_data = refs_render_data ~only_public refs db in
    write_page ~dir g (Year_html.page g ~year render_data)

  let write_years ~dir db g =
    let years = Year.public_domain_stmt in
    let* years = Db.list db years |> Db.string_error in
    let index = Year_html.index g years in
    let* () = write_page ~dir g index in
    let write_year = write_year ~dir db g in
    let* () = List.iter_stop_on_error write_year years in
    Ok ()

  let write_bib_pages ~dir _db g =
    let* () = write_page ~dir g (Home_html.page g) in
    let* () = write_page ~dir g (Help_html.page g) in
    Ok ()

  let write_static_files ~dir conf =
    let src = Cli_kit.Conf.static_dir conf in
    Bazaar.cp_dir_content ~recurse:true ~of_dir:src ~inside_dir:dir ()

  let write_data ~dir data_conf db g =
    let* () = write_references ~dir db g in
    let* () = write_containers ~dir db g in
    let* () = write_persons ~dir db g in
    let* () = write_subjects ~dir db g in
    let* () = write_years ~dir db g in
    let* () = write_bib_pages ~dir db g in
    let* () = write_static_files ~dir data_conf in
    Ok ()
end

let static_html ~inside_dir data_conf db g =
  Static_html.write_data ~dir:inside_dir data_conf db g

module Bibtex = struct
  type type' =
  [ `Article | `Book | `Booklet
  | `Conference (* Use inproceedings *) | `Inbook | `Incollection
  | `Inproceedings | `Manual | `Mastersthesis | `Misc | `Phdthesis
  | `Proceedings | `Techreport | `Unpublished ]

  let type_to_string = function
  | `Article -> "article" | `Book -> "book" | `Booklet -> "booklet"
  | `Conference -> "conference" | `Inbook -> "inbook"
  | `Incollection -> "incollection" | `Inproceedings -> "inproceedings"
  | `Manual -> "manual" | `Mastersthesis -> "mastersthesis"
  | `Misc -> "misc" | `Phdthesis -> "phdthesis" | `Proceedings -> "proceedings"
  | `Techreport -> "techreport" | `Unpublished -> "unpublished"

 let type_of_crossref_type :
   string -> [ `Article | `Book | `Inproceedings | `Phdthesis | `Misc ] =
   function
   | "book" -> `Book
   | "book-chapter" | "book-part" | "book-section" ->
       (* This seems more adapted than `Inbook if you have a chap. title. *)
       `Inproceedings
   | "book-series" | "book-set" | "book-track" -> `Book
   | "component" -> `Article
   | "dataset" -> `Misc
   | "dissertation" -> `Phdthesis
   | "edited-book" -> `Book
   | "journal" -> `Book
   | "journal-article" -> `Article
   | "journal-issue" -> `Book
   | "journal-volume" -> `Book
   | "monograph" -> `Book
   | "other" -> `Misc
   | "peer-review" -> `Misc
   | "posted-content" -> `Article (* is that pre-prints (?) *)
   | "proceedings" -> `Book
   | "proceedings-article" -> `Inproceedings
   | "proceedings-series" -> `Book
   | "reference-book" -> `Book
   | "reference-entry" -> `Book
   | "report" -> `Book
   | "report-series" -> `Book
   | "standard" -> `Book
   | "standard-series" -> `Book
   | _ -> `Misc

  let lowercase = String.Ascii.lowercase

  (* Generate citation keys.

     Tries do find a scheme that is user-friendly, stable with new additions and
     doesn't generate collisions. We use a nameYYYYtoken-token scheme with
     name the family name of the first author mangled to the US-ASCII
     charset, YYYY the publication year, and the two tokens selected
     pseudo-randomly from the title itself. *)

  let cite_key_part_of_author rs r =
    let mangle n = (* TODO improve *)
      if Char.Ascii.is_white n then '-' else
      if not (Char.Ascii.is_alphanum n) then 'X' else n
    in
    let name =
      match
        Reference.Id.Map.get_list (Reference.id r) rs.Reference.authors
      with
      | [] -> "Anonymous"
      | auths -> Person.last_name @@ List.hd auths
    in
    String.map mangle (String.Ascii.lowercase name)

  let cite_key_part_of_date r = match Reference.date r with
  | None -> "YYYY" | Some (y, _) -> Fmt.str "%d" y

  let cite_key_part_of_title r =
    let minstd n = (48_271 * n) mod 0x7fffffff in
    let sum_bytes s =
      let acc = ref 0 in
      for i = 0 to String.length s - 1 do acc := !acc + Char.code s.[i] done;
      !acc
    in
    let select ~min_len s =
      let s = String.trim s in
      if not (String.length s >= min_len) then None else
      if not (String.for_all Char.Ascii.is_alphanum s) then None else
      Some (String.Ascii.lowercase s)
    in
    let title = match Reference.title r with "" -> "Untitled" | t -> t in
    let tokens = String.split_on_char ' ' title in
    let token_candidates = match List.filter_map (select ~min_len:5) tokens with
    | [] -> List.filter_map (select ~min_len:4) tokens | ts -> ts
    in
    match token_candidates with
    | [] -> "" | [t] -> t
    | ts ->
        let len = List.length ts in
        let idx0 = minstd (sum_bytes title) in
        let idx1 = minstd idx0 in
        let idx0 = idx0 mod len and idx1 = idx1 mod len in
        let idx1 = if idx0 = idx1 then (idx0 + 1) mod len else idx1 in
        let idx0, idx1 = if idx0 > idx1 then idx1, idx0 else idx0, idx1 in
        Fmt.str "%s-%s" (List.nth ts idx0) (List.nth ts idx1)

  let ref_to_cite_key rs r =
    Fmt.str "%s%s%s"
      (cite_key_part_of_author rs r)
      (cite_key_part_of_date r)
      (cite_key_part_of_title r)

  (* Other fields. *)

  let person_to_bib p = match Person.last_name p, Person.first_names p with
  | name, "" -> name | "", name -> name
  | last, firsts -> Fmt.str "%s %s" firsts last

  let ref_author rs r =
    match Reference.Id.Map.get_list (Reference.id r) rs.Reference.authors with
    | [] -> ""
    | authors -> String.concat " and " (List.map person_to_bib authors)

  let ref_editor rs r =
    match Reference.Id.Map.get_list (Reference.id r) rs.Reference.editors with
    | [] -> ""
    | editors -> String.concat " and " (List.map person_to_bib editors)

  let ref_keywords rs r =
    match Reference.Id.Map.get_list (Reference.id r) rs.Reference.subjects with
    | [] -> "" | ss -> String.concat ", " (List.map Subject.name ss)

  let ref_year r = match Reference.date r with
  | Some (y, _) -> Fmt.str "%d" y | None -> ""

  let ref_month r = match Reference.date r with
  | Some (_, (Some (m, _))) -> Fmt.str "%d" m | _ -> ""

  let ref_to_bib rs r =
    let add_if_non_empty k v m = if v = "" then m else String.Map.add k v m in
    let add_if_some k v m = match v with
    | None -> m | Some v -> String.Map.add k v m
    in
    let container =
      let find_container c =
        Container.Id.Map.find_opt c rs.Reference.containers
      in
      Option.bind (Reference.container r) find_container
    in
    let container_title =
      Option.value ~default:"" (Option.map Container.title container)
    in
    let cite_key = ref_to_cite_key rs r in
    let base_fields =
      String.Map.empty
      |> add_if_non_empty "note" (Reference.note r)
      |> add_if_non_empty "author" (ref_author rs r)
      |> add_if_some "doi" (Reference.doi r)
      |> add_if_non_empty "editor" (ref_editor rs r)
      |> add_if_non_empty "keywords" (ref_keywords rs r)
      |> add_if_non_empty "number" (Reference.issue r)
      |> add_if_non_empty "pages" (Reference.pages r)
      |> add_if_non_empty "publisher" (Reference.publisher r)
      |> add_if_non_empty "title" (Reference.title r)
      |> add_if_non_empty "volume" (Reference.volume r)
      |> add_if_non_empty "year" (ref_year r)
      |> add_if_non_empty "month" (ref_month r)
    in
    let type', fields =
      let t = type_of_crossref_type (Reference.type' r) in
      match t with
      | `Article ->
          t, base_fields |> String.Map.add "journal" container_title
      | `Book ->
          t, base_fields |> add_if_non_empty "series" container_title
      | `Inproceedings ->
          t, base_fields |> add_if_non_empty "booktitle" container_title
      | `Phdthesis ->
          t, base_fields
      | `Misc ->
          t, base_fields
    in
    Bibtex.v ~type':(type_to_string type') ~cite_key ~fields ()

  let of_refs ~now bib_conf rrender_data =
    let t = Bibliography.project_title bib_conf in
    let href = Bibliography.project_href bib_conf in
    let preamble = Fmt.str "%% %s\n%% %s" t href in
    let now = Ptime.to_rfc3339 ~space:true ~tz_offset_s:3600 now in
    let bibs = List.map (ref_to_bib rrender_data) rrender_data.Reference.list in
    let sep = Fmt.(cut ++ cut) in
    Ok (Fmt.str "@[<v>%a@,%% Generated on %s@,@,%a@]"
          Fmt.lines preamble now (Fmt.list ~sep Bibtex.pp) bibs)
end

let bibtex_of_refs = Bibtex.of_refs

(*
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
*)
