(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let menu_items =
  [ "References", Kurl.V (Legacy.Page_url.kind, Refs);
    "Containers", Kurl.V (Legacy.Page_url.kind, Containers);
    "Persons", Kurl.V (Legacy.Page_url.kind, Persons);
    "Subjects", Kurl.V (Legacy.Page_url.kind, Subjects);
    "Years", Kurl.V (Legacy.Page_url.kind, Years); ]

let page_html g ~self ~title ~content =
  let b = Page.Gen.bibliography g in
  let proj = Bibliography.project_title b in
  let title = if title = proj then proj else String.concat " – " [title;proj] in
  let body = Page.body g ~ui:(Page.ui g ~menu_items ~self) ~content in
  let doc = Page.frame g ~self ~title ~body in
  Page.v ~url:self ~doc ~part:false

module Container' = struct
  let href rf = Kurl.Fmt.url rf (Kurl.v Legacy.Page_url.kind Containers)
  let href rf ~id = String.concat "#" [href rf; id]

  let id title =
    let sp_to_dash = function ' ' -> '-' | c -> c in
    String.map sp_to_dash title

  let name t = El.span ~at:[Hclass.container] [El.txt t]

  let link rf t =
    let href = href rf (id t) in
    Hfrag.link ~at:[Hclass.container] ~href (name t)

  let page ~ref_list_html g refdb =
    let uf = Page.Gen.url_fmt g in
    let container_index js =
      let letter_id l = Fmt.str "journal-%s" l in
      let letter_link (l, _) =
        El.a ~at:At.[Hfrag.anchor_href (letter_id l)] El.[txt l]
      in
      let letter_section (l, ps) =
        let ps = List.sort compare ps in
        let letter_id = letter_id l in
        let container_li t =
          let cid = id t and c = name t in
          let anchor_id = Fmt.str "%s-%s" letter_id cid in
          El.li ~at:At.[id anchor_id]
            [Hfrag.anchor_a anchor_id;
             El.a ~at:At.[class' "ref-container"; Hfrag.anchor_href cid] [c]]
        in
        El.splice @@
        [ El.h2 ~at:At.[id letter_id] [Hfrag.anchor_a letter_id; El.txt l];
          El.ol (List.map container_li ps) ]
      in
      let classes j =
        if j = "" then [] else
        [String.of_char (Char.Ascii.lowercase j.[0])]
      in
      let classes = List.classify ~classes js in
      El.nav ~at:At.[class' "container-index"] [
        El.nav ~at:At.[class' "letter-index"] (List.map letter_link classes);
        El.splice (List.map letter_section classes)]
    in
    let container_refs db t =
      let refs = Legacy.Refdb.find_container_title db t in
      let count = Hfrag.item_count (List.length refs) in
      let container_id = id t in
      [ El.h2 ~at:At.[id container_id]
          [Hfrag.anchor_a container_id; name t; El.sp; count];
        ref_list_html uf db refs ]
    in
    let containers = List.rev (Legacy.Refdb.container_title_index refdb) in
    let count = Hfrag.item_count (List.length containers) in
    let content =
      El.section [
        El.h1 [El.txt "Journals "; count];
        El.p [El.txt "Classified by journal or container title."];
        container_index containers;
        El.splice (List.concat_map (container_refs refdb) containers)]
    in
    let self = Kurl.V (Legacy.Page_url.kind, Containers) in
    page_html g ~self ~title:"Containers" ~content
end

module Subject' = struct
  let href uf = Kurl.Fmt.url uf (Kurl.v Legacy.Page_url.kind Subjects)
  let href uf ~id = String.concat "#" [href uf; id]
  let id s =
    let sp_to_dash = function ' ' -> '-' | c -> c in
    String.map sp_to_dash (Legacy.Subject'.name s)

  let subject_parent_map sdb =
    let sort_by_name (s, _) (s', _) = Legacy.Subject'.compare_by_name s s' in
    let add_children acc r = (r, Legacy.Subject'.Db.find_children r sdb) :: acc
    in
    let m = List.fold_left add_children [] (Legacy.Subject'.Db.roots sdb) in
    List.sort sort_by_name m

  let page ~ref_list_html g refdb =
    let uf = Page.Gen.url_fmt g in
    let subject_descr s = match Legacy.Subject'.description s with
    | None -> El.void | Some d -> El.p ~at:At.[class' "subject-descr"]
                                    [El.txt d]
    in
    let linked_subject s =
      let href = href uf (id s) in
      Hfrag.link
        ~at:At.[class' "see-subject"] ~href (El.txt_of Legacy.Subject'.name s)
    in
    let see_also sdb s = match Legacy.Subject'.Db.find_see_also s sdb with
    | [] -> El.void
    | ss -> El.splice
              El.[txt "See also ";
                  El.splice ~sep:(El.txt ", ") (List.map linked_subject ss)]
    in
    let subject_index sdb =
      let subject_anchor_id s = Fmt.str "subject-%s" (id s) in
      let subject s =
        let sid = id s in
        let n = Legacy.Subject'.name s in
        El.a ~at:At.[class' "ref-subject"; Hfrag.anchor_href sid] [El.txt n]
      in
      let root_subject (p, children) =
        let sid = subject_anchor_id p in
        let children = List.sort Legacy.Subject'.compare_by_name children in
        let child s =
          let sid = subject_anchor_id s in
          El.li ~at:At.[id sid] [Hfrag.anchor_a sid; subject s]
        in
        El.splice @@
        [El.h2 ~at:At.[id sid] [Hfrag.anchor_a sid; subject p];
         subject_descr p; El.ol (List.map child children);]
      in
      let smap = subject_parent_map sdb in
      El.nav ~at:At.[class' "subject-index"] (List.map root_subject smap)
    in
    let subject_refs db k =
      let refs = Legacy.Refdb.find_subject db k in
      let count = Hfrag.item_count (List.length refs) in
      let subject_id = id k in
      let name = Legacy.Subject'.name k in
      [ El.h2 ~at:At.[id subject_id]
          [Hfrag.anchor_a subject_id; El.txt (Fmt.str "%s " name);
           count; see_also (Legacy.Refdb.subjects db) k];
        subject_descr k;
        ref_list_html uf db refs ]
    in
    let sdb = Legacy.Refdb.subjects refdb in
    let all_subjects = Legacy.Subject'.Db.all sdb in
    let all_subjects = List.sort Legacy.Subject'.compare_by_name all_subjects in
    let count = Hfrag.item_count (List.length all_subjects) in
    let content =
      El.section [
        El.h1 [El.txt "Subjects "; count];
        subject_index sdb;
        El.splice (List.concat_map (subject_refs refdb) all_subjects)]
    in
    let self = Kurl.V (Legacy.Page_url.kind, Legacy.Page_url.Subjects) in
    page_html g ~self ~title:"Subjects" ~content
end

module Person' = struct
  let href uf = Kurl.Fmt.url uf (Kurl.v Legacy.Page_url.kind Persons)
  let href uf ~id = String.concat "#" [href uf; id]
  let id p =
    let sp_to_dash = function ' ' -> '-' | c -> c in
    let giv = match Legacy.Person'.given p with
    | None -> "" | Some uf -> "_" ^ uf
    in
    String.map sp_to_dash (Legacy.Person'.family p ^ giv)

  let names p =
    let giv = match Legacy.Person'.given p with
    | None -> "" | Some g -> ", " ^ g in
    El.txt (Legacy.Person'.family p ^ giv)

  let link uf p =
    let href = href uf (id p) in
    Hfrag.link ~at:[Hclass.person] ~href (names p)

  let inline_list uf ps = match ps with
  | [] -> El.txt "Unknown"
  | ps -> El.splice ~sep:(El.txt ", ") (List.map (link uf) ps)

  let orcid p = match Legacy.Person'.orcid p with
  | None -> El.void
  | Some orcid ->
      El.splice [El.sp; Hfrag.link ~at:At.[class'  "orcid"]
                   ~href:orcid (El.txt "ORCID")]

  let page ~ref_list_html g refdb =
    let uf = Page.Gen.url_fmt g in
    let person_index ps =
      let letter_id l = Fmt.str "person-%s" l in
      let letter_link (l, _) =
        El.a ~at:At.[Hfrag.anchor_href (letter_id l)] El.[txt l]
      in
      let letter_section (l, ps) =
        let compare p0 p1 =
          String.compare
            (Legacy.Person'.family_term p0)
            (Legacy.Person'.family_term p1)
        in
        let ps = List.sort compare ps in
        let letter_id = letter_id l in
        let person_li p =
          let pid = id p in
          let anchor_id = Fmt.str "%s-%s" letter_id pid in
          El.li ~at:At.[id anchor_id]
            [Hfrag.anchor_a anchor_id;
             El.a ~at:At.[class' "ref-person"; Hfrag.anchor_href pid] [names p];
             orcid p ]
        in
        El.splice @@
        [ El.h2 ~at:At.[id letter_id] [Hfrag.anchor_a letter_id; El.txt l];
          El.ol (List.map person_li ps) ]
      in
      let classes p =
        let fam = Legacy.Person'.family_term p in
        if fam = "" then [] else
        [String.of_char (Char.Ascii.lowercase fam.[0])]
      in
      let classes = List.classify ~classes ps in
      El.nav ~at:At.[class' "person-index"] [
        El.nav ~at:At.[class' "letter-index"] (List.map letter_link classes);
        El.splice (List.map letter_section classes)]
    in
    let person_refs db p =
      let refs = Legacy.Refdb.find_person db p in
      let count = Hfrag.item_count (List.length refs) in
      let person_id = id p in
      [ El.h2 ~at:At.[id person_id]
          [Hfrag.anchor_a person_id; names p; El.sp; orcid p; El.sp; count];
        ref_list_html uf db refs ]
  in
  let persons = Legacy.Refdb.person_index refdb in
  let count = Hfrag.item_count (List.length persons) in
  let content =
    El.section [
      El.h1 [El.txt "Person "; count];
      El.p [El.txt "Classified by person, in author or editor position."];
      person_index persons;
      El.splice (List.concat_map (person_refs refdb) persons)]
  in
  let self = Kurl.V (Legacy.Page_url.kind, Persons) in
  page_html g ~self ~title:"Persons" ~content
end

module Year' = struct
  let href uf = Kurl.Fmt.url uf (Kurl.v Legacy.Page_url.kind Years)
  let href uf ~id = String.concat "#" [href uf; id]
  let id = function 0 -> "????" | y -> Fmt.str "%d" y

  let page ~ref_list_html g refdb =
    let uf = Page.Gen.url_fmt g in
    let year_index ys =
      let year y =
        let y = id y in
        El.a ~at:At.[Hfrag.anchor_href y] [El.txt y]
      in
      El.nav ~at:At.[class' "year-index"] (List.map year ys)
    in
    let year_refs db y =
      let refs = Legacy.Refdb.find_year db y in
      let count = Hfrag.item_count (List.length refs) in
      let yid = id y in
      [ El.h2 ~at:At.[id yid]
          [ Hfrag.anchor_a yid; El.txt (Fmt.str "%d " y); count ];
        ref_list_html uf db refs ]
    in
    let years = Legacy.Refdb.year_index refdb in
    let count = Hfrag.item_count (List.length years) in
    let content =
      El.section [
        El.h1 [El.txt "Years "; count];
        El.p [El.txt "Classified by year of earliest, online or print, \
                      publication date."];
        year_index years;
        El.splice (List.concat_map (year_refs refdb) years); ]
    in
    let self = Kurl.V (Legacy.Page_url.kind, Years) in
    page_html g ~self ~title:"Years" ~content
end

module Reference' = struct
  let href uf = Kurl.Fmt.url uf (Kurl.v Legacy.Page_url.kind Refs)
  let href uf ~id = String.concat "#" [href uf; id]
  let id r = match Legacy.Reference'.doi r with
  | None -> "id"  (* FIXME *) | Some doi -> doi

  let link_year uf y =
    Hfrag.link ~at:[Hclass.year] ~href:(Year'.href uf y) (El.txt y)

  let ref_year uf r =
    El.splice
      [ link_year uf (Year'.id (Legacy.Reference'.year r)); El.txt " "]

  let ref_authors uf r =
    let authors =
      Person'.inline_list uf (Legacy.Reference'.authors r)
    in
    El.splice [ El.span ~at:At.[class' "ref-authors"] [authors]; El.sp ]

  let ref_title r =
    let t = match Legacy.Reference'.title r with
    | None -> "Untitled" | Some t -> t
    in
    El.span ~at:At.[class' "ref-title"] [El.txt t; El.txt ". "]

  let ref_vol_issue r =
    match Legacy.Reference'.volume r, Legacy.Reference'.issue r with
    | None, None -> El.void
    | Some v, Some i -> El.splice [ El.txt (Fmt.str ", %s(%s)" v i) ]
    | Some n, None | None, Some n -> El.splice [ El.txt (Fmt.str ", %s" n) ]

  let ref_pages r = match Legacy.Reference'.pages r with
  | None -> El.void | Some pp -> El.txt (Fmt.str ", %s" pp)

  let ref_editors uf r = match Legacy.Reference'.editors r with
  | [] -> El.void
  | editors ->
      El.splice
        [ El.txt "In "; El.span ~at:At.[class' "ref-editors"]
            [Person'.inline_list uf editors]]

  let ref_container uf r = match Legacy.Reference'.container_title r with
  | None -> El.void
  | Some t ->
      El.span ~at:At.[class' "ref-container"]
        [ ref_editors uf r; El.sp; Container'.link uf t;
          (* ref_vol_issue r; ref_pages r; *)
          El.txt ". "]

  let ref_full_text r = match Legacy.Reference'.doi r with
  | None -> El.void
  | Some doi ->
      let dlink = Fmt.str "%s/%s" Doi.default_resolver doi in
      let at = At.[class' "ref-doi"; href dlink; v "data-doi" doi] in
      El.a ~at [El.txt "Full text"]

  let ref_citations uf db r = match Legacy.Refdb.citations db r with
  | [] -> El.void
  | cs ->
      let cite_ref r =
        let p = match Legacy.Reference'.authors r with
        | [] -> "Unknown"
        | p :: _ -> Legacy.Person'.family p
        in
        let cite =
          El.txt (Fmt.str "%s %s" p (Year'.id (Legacy.Reference'.year r)))
        in
        let link_ref r =
          let href = href uf (id r) in
          Hfrag.link ~at:At.[class' "ref-cite"] ~href cite
        in
        link_ref r
      in
      let citations = El.splice ~sep:El.sp (List.map cite_ref cs) in
      let label = El.summary ~at:At.[class' "label"] [El.txt "cites "] in
      El.details ~at:At.[class' "ref-cites"] [label; El.p [citations]]

  let ref_subjects uf db r = match Legacy.Reference'.subjects r with
  | [] -> El.void
  | ss ->
      let subject k =
        match Legacy.Subject'.Db.find k (Legacy.Refdb.subjects db) with
        | None -> []
        | Some vs ->
            (* Remove ambiguous stuff *)
            let keep s = List.mem r (Legacy.Refdb.find_subject db s) in
            List.filter keep vs
      in
      let ss = List.concat_map subject ss in
      let subj s =
        let href = Subject'.href uf (Subject'.id s) in
        Hfrag.link ~at:[Hclass.subject] ~href (El.txt_of Legacy.Subject'.name s)
      in
      El.p ~at:At.[class' "ref-subjects"]
        [El.splice ~sep:El.sp (List.map subj ss)]

  let ref_note r = match Legacy.Reference'.note r with
  | None | Some "" -> El.void
  | Some note ->
      let note = El.p [El.txt note] in
      let label = El.summary ~at:At.[class' "label"] [El.txt "note"] in
      El.details ~at:At.[class' "ref-note"] [label; note ]

  let ref_ref uf r =
    El.p ~at:At.[class' "ref"]
      [ ref_title r; El.sp;
        ref_container uf r; ref_year uf r; ref_authors uf r; ]

  let ref_details uf db r =
    El.div ~at:At.[class' "ref-details"]
      [ ref_full_text r;
        ref_citations uf db r;
        ref_note r; ]

  let item uf db r =
    let ref_anchor_id = id r in
    El.li ~at:At.[class' "ref-item"; id ref_anchor_id]
      [ Hfrag.anchor_a ref_anchor_id; ref_ref uf r;
        ref_subjects uf db r;
        ref_details uf db r; ]

  let list_by_desc_date uf db refs =
    let refs = List.sort (Fun.flip Legacy.Reference'.compare_by_date) refs in
    El.ol ~at:At.[class' "ref-list"] (List.map (item uf db) refs)

  let page g refdb =
    let uf = Page.Gen.url_fmt g in
    let refs = Legacy.Refdb.refs refdb in
    let count = Hfrag.item_count (List.length refs) in
    let content =
      El.section [
        El.h1 [El.txt "References "; count];
        El.p [ El.txt "All references in the corpus, listed by decreasing \
                       publication date."];
        list_by_desc_date uf refdb refs; ]
    in
    let self = Kurl.V (Legacy.Page_url.kind, Refs) in
    page_html g ~self ~title:"References" ~content
end

let help_page g _ =
  let self = Kurl.V (Legacy.Page_url.kind, Help) in
  page_html g ~self ~title:"Help" ~content:(Help_html.page_html g)

let home_page g _ =
  let self = Kurl.V (Legacy.Page_url.kind, Home) in
  page_html g ~self ~title:"About" ~content:(Home_html.page_html g)

module Gen_bib = struct

  let lowercase = String.Ascii.lowercase

  let get_authors r = match Legacy.Reference'.authors r with
  | [] -> [Legacy.Person'.anonymous]
  | auths -> auths

  let get_title r = match Legacy.Reference'.title r with
  | None -> "Untitled" | Some t ->  t

  (* Generate citation keys.

     Tries do find a scheme that is user-friendly, stable with new additions and
     doesn't generate collisions. We use a nameYYYYtoken-token scheme with
     name the family name of the first author mangled to the US-ASCII
     charset, YYYY the publication year, and the two tokens selected
     pseudo-randomly from the title itself. *)

  let cite_key_part_of_author r =
    let mangle n = (* TODO improve *)
      if Char.Ascii.is_white n then '-' else
      if not (Char.Ascii.is_print n) then 'X' else n
    in
    let name = Legacy.Person'.family @@ List.hd (get_authors r) in
    String.map mangle (String.Ascii.lowercase name)

  let cite_key_part_of_date r = match Legacy.Reference'.date r with
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
    let title = get_title r in
    let tokens = String.split_on_char ' ' (get_title r) in
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

  let ref_to_cite_key r =
    Fmt.str "%s%s%s"
      (cite_key_part_of_author r)
      (cite_key_part_of_date r)
      (cite_key_part_of_title r)

  (* Other fields. *)

  let person_to_bib p =
    match Legacy.Person'.family p, Legacy.Person'.given p with
    | name, Some given -> Fmt.str "%s %s" given name
    | name, None -> name

  let ref_author r = match get_authors r with
  | [] -> None
  | authors -> Some (String.concat " and " (List.map person_to_bib authors))

  let ref_keywords r = match Legacy.Reference'.subjects r with
  | [] -> None | ks -> Some (String.concat ", " ks)

  let ref_year r = match Legacy.Reference'.date r with
  | None -> None | Some (y, _) -> Some (Fmt.str "%d" y)

  let ref_to_bib r =
    let add_if_some k v m = match v with
    | None -> m | Some v -> String.Map.add k v m
    in
    let cite_key = ref_to_cite_key r in
    let type' = match Legacy.Reference'.container_title r with
    | None -> "misc" | Some _ -> "article"
    in
    let fields =
      String.Map.empty
      |> add_if_some "annote" (Legacy.Reference'.note r)
      |> add_if_some "author" (ref_author r)
      |> add_if_some "journal" (Legacy.Reference'.container_title r)
      |> add_if_some "doi" (Legacy.Reference'.doi r)
      |> add_if_some "keywords" (ref_keywords r)
      |> add_if_some "number" (Legacy.Reference'.issue r)
      |> add_if_some "pages" (Legacy.Reference'.pages r)
      |> add_if_some "title" (Legacy.Reference'.title r)
      |> add_if_some "volume" (Legacy.Reference'.volume r)
      |> add_if_some "year" (ref_year r)
    in
    Bibtex.v ~type' ~cite_key ~fields ()

  let of_ref_list ~preamble ~now refs =
    let now = Ptime.to_rfc3339 ~space:true ~tz_offset_s:3600 now in
    let bibs = List.map ref_to_bib refs in
    let sep = Fmt.(cut ++ cut) in
    Ok (Fmt.str "@[<v>%a@,%% Generated on %s@,@,%a@]"
          Fmt.lines preamble now (Fmt.list ~sep Bibtex.pp) bibs)
end


module Gen_html = struct
  open Result.Syntax

  let filepath uf u =
    let uf = Kurl.Fmt.with_fmt ~use_exts:true uf in
    let file = Kurl.Fmt.url uf u in
    let file = String.subrange ~first:1 (* drop / *) file in
    Fpath.of_string file

  let page_list =
    let ref_list_html = Reference'.list_by_desc_date in
    [ Container'.page ~ref_list_html;
      help_page;
      home_page;
      Person'.page ~ref_list_html;
      Reference'.page;
      Subject'.page ~ref_list_html;
      Year'.page ~ref_list_html; ]

  let write_page ~dir g refdb page =
    let p = page g refdb in
    let* file = filepath (Page.Gen.url_fmt g) (Page.url p) in
    let doc = Page.doc_to_string p in
    Os.File.write ~force:true ~make_path:true Fpath.(dir // file) doc

  let rec write_pages ~dir g refdb = function
  | [] -> Ok ()
  | p :: ps ->
      let* () = write_page ~dir g refdb p in
      write_pages ~dir g refdb ps

  let write_bib_file ~dir g refdb =
    let t = Bibliography.project_title (Page.Gen.bibliography g) in
    let href = Bibliography.project_href (Page.Gen.bibliography g) in
    let preamble = Fmt.str "%% %s\n%% %s" t href in
    let now = Page.Gen.now g and refs = Legacy.Refdb.refs refdb in
    let* bib = Gen_bib.of_ref_list ~preamble ~now refs in
    let file = Fpath.(dir / "philoclimate.bib") in
    Os.File.write ~force:true ~make_path:true file bib

  let gen ~dir g refdb =
    let* () = write_pages ~dir g refdb page_list in
    let* () = write_bib_file ~dir g refdb in
    Ok ()
end

let gen_html = Gen_html.gen

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
