(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let find_container r rs =
  let find_container c = Id.Map.find_opt c rs.Reference.containers in
  Option.bind (Reference.container r) find_container

let find_authors r rs = Id.Map.get_list (Reference.id r) rs.Reference.authors
let find_editors r rs = Id.Map.get_list (Reference.id r) rs.Reference.editors
let find_subjects r rs = Id.Map.get_list (Reference.id r) rs.Reference.subjects
let cites_anchor = "cites"
let cited_by_anchor = "cited-by"

let viz = Entity_html.viz (module Reference)

let ui_ext g ~self =
  if not (Page.Gen.editable g) then El.void else
  let new_button =
    let uf = Page.Gen.url_fmt g in
    let cancel = Some (Kurl.Fmt.url uf self) in
    let dst = Reference.Url.v (New_form { cancel }) in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
    Hfrag.new_entity_button ~href ~label:Uimsg.new_reference
  in
  Hui.group ~at:At.[Hclass.entity_menu] ~dir:`H [new_button]

let h1_reference uf ~self ?title r =
  let entity_kind =
    let kind = Uimsg.references in
    Hfrag.entity_kind_index uf ~self ~kind (Reference.Url.v Index)
  in
  El.h1 [entity_kind]

let entity_cancel_button uf s =
  let cancel = Reference.Url.v (View_fields (Reference.id s)) in
  Hfrag.hc_cancel_button uf cancel

let confirm_delete g r =
  let self = Reference.Url.page r in
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_reference uf ~self r in
  let cancel_button = entity_cancel_button uf r in
  let delete_button =
    let confirm = Reference.Url.v (Delete (Reference.id r)) in
    let target = Hfrag.target_entity_up in
    Hfrag.hc_delete uf confirm ~target (El.txt Uimsg.confirm_delete)
  in
  let bs = Hui.group ~align:`Justify ~dir:`H [delete_button; cancel_button] in
  let really =
    El.p [El.txt_of Uimsg.really_delete_reference (Reference.title r)]
  in
  let no_undo_warn =
    El.p ~at:[Hclass.message; Hclass.warn] [El.txt Uimsg.this_cannot_be_undone]
  in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [h1; really; no_undo_warn; bs]

let edit_title r =
  let label = El.txt Uimsg.title and col = Reference.title' in
  Hui.field_text ~autogrow:true ~min_rows:1 ~label ~col r

let edit_doi r =
  let label = El.txt Uimsg.doi and col = Reference.doi' in
  Hui.field_string ~autogrow:true ~min_size:8 ~label ~col r

let edit_isbn r =
  let label = El.txt Uimsg.isbn and col = Reference.isbn' in
  Hui.field_string ~autogrow:true ~min_size:8 ~label ~col r

let edit_container uf r c =
  let input_name = Rel.Col.name Reference.container' in
  let input = match c with
  | None -> Entity_html.container_input_finder uf ~input_name
  | Some (`Exists c) -> Entity_html.container_input uf ~input_name c
  | Some (`To_create c) -> Entity_html.container_input_create uf ~input_name c
  in
  let span = El.span ~at:[Hui.Class.label] [El.txt Uimsg.container] in
  let at = [Hclass.field; Hui.Class.for_col Reference.container'] in
  El.label ~at [span; input]

let edit_date r =
  let label = El.txt Uimsg.date in
  let v = match Reference.date r with
  | None -> "" | Some d -> Date.partial_to_string d
  in
  let name = Hquery.date_key in
  Hui.field_string' ~autogrow:true ~min_size:10 ~name ~label v

let edit_volume r =
  let label = El.txt Uimsg.volume and col = Reference.volume' in
  Hui.field_string ~autogrow:true ~min_size:4 ~label ~col r

let edit_issue r =
  let label = El.txt Uimsg.issue in
  Hui.field_string ~autogrow:true ~min_size:4 ~label ~col:Reference.issue' r

let edit_pages r =
  let label = El.txt Uimsg.pages in
  Hui.field_string ~autogrow:true ~min_size:4 ~label ~col:Reference.pages' r

let edit_publisher r =
  let label = El.txt Uimsg.publisher in
  let col = Reference.publisher' in
  Hui.field_string ~autogrow:true ~min_size:10 ~label ~col r

let edit_note = Entity_html.edit_note (module Reference)
let edit_private_note = Entity_html.edit_private_note (module Reference)
let edit_public = Entity_html.edit_public (module Reference)
let edit_make
    (type t) (module E : Entity.PUBLICABLE with type t = t) ?(at = [])  e
  =
  let label = El.txt Uimsg.public in
  Hui.field_bool ~label ~col:E.public' e

let edit_type r =
  let label = El.txt Uimsg.type' in
  let options = List.map fst Crossref.types in
  let option_value = Fun.id in
  let option_text t = match List.assoc_opt t Crossref.types with
  | None -> Uimsg.other | Some l -> l
  in
  let col = Reference.type'' in
  Hui.field_select ~label ~option_text ~option_value ~options ~col r

let edit_contributor role uf ps =
  let at = [Hclass.field; Hui.Class.for_table Reference.Contributor.table]
  in
  let for_list = true and input_name = Hquery.person_key role in
  let contributor = function
  | `To_create p ->
      Entity_html.person_input_create uf ~for_list ~input_name ~role p
  | `Exists p ->
      Entity_html.person_input uf ~for_list ~input_name ~role p
  in
  let label = match role with
  | Some Person.Author -> Uimsg.authors
  | Some Person.Editor -> Uimsg.editors
  | None -> Uimsg.persons
  in
  let label = El.span ~at:[Hui.Class.label] [El.txt label] in
  let finder = Entity_html.person_input_finder uf ~for_list ~input_name ~role in
  let ps = List.map contributor ps in
  let ps = El.div ~at:[Hui.Class.ordering; Hclass.list] (ps @ [finder]) in
  El.div ~at [label; ps]

let edit_subjects uf ss =
  let at = [Hclass.field; Hui.Class.for_table Reference.Subject.table] in
  let label = El.span ~at:[Hui.Class.label] [El.txt Uimsg.subjects] in
  let input_name = Reference.Subject.(Hquery.key_for_rel table subject') in
  let for_list = true in
  let ss = List.map (Entity_html.subject_input uf ~for_list ~input_name) ss in
  let add = Entity_html.subject_input_finder uf ~for_list ~input_name in
  El.div ~at [label; El.div ~at:[Hclass.list (* bad *)] (ss @ [add])]

let edit_submit uf ~submit r =
  let url, label = match submit with
  | `New _ -> Reference.Url.v Create, Uimsg.create_reference
  | `Edit -> Reference.Url.v (Update (Reference.id r)), Uimsg.save_reference
  in
  let r = Hfrag.hc_request uf url and e = Hc.effect `Element in
  let q = Hc.query "form:up" and rescue = Hc.query_rescue true in
  let t = Hc.target ":up :up :up :up" in
  let at = At.[t; r; e; q; rescue; Hui.Class.submit] in
  Hui.button ~at (El.txt label)

let edit_cancel uf ~submit r = match submit with
| `Edit
| `New None -> entity_cancel_button uf r
| `New (Some cancel_href) ->
    Hui.button_link ~href:cancel_href (El.txt Uimsg.cancel)

let edit_buttons uf ~submit r =
  let cancel = edit_cancel uf ~submit r in
  let submit = edit_submit uf ~submit r in
  Hui.group ~align:`Justify ~dir:`H [cancel; submit]

let edit_cites cites = (* Hidden for now *)
  let cite doi = El.input ~at:At.[hidden; name Hquery.cite_key; value doi] () in
  El.splice (List.map cite cites)

let edit_reference
    g ~self ~submit r ~authors ~editors ~subjects ~container ~cites =
  let uf = Page.Gen.url_fmt g in
  let title = edit_title r in
  let details =
    let open' = match submit with `New _ -> At.true' "open" | _ -> At.void in
    let summary = El.summary ~at:[Hui.Class.label] [El.txt Uimsg.details] in
    let ref_ids =
      let doi = edit_doi r in
      let isbn = edit_isbn r in
      let type' = edit_type r in
      Hui.group ~at:[Hclass.reference_ids] ~dir:`H [doi; isbn; type']
    in
    let cites = edit_cites cites in
    let container_title = edit_container uf r container in
    let container_details =
      let date = edit_date r in
      let volume = edit_volume r in
      let issue = edit_issue r in
      let pages = edit_pages r in
      Hui.group ~at:[Hclass.container_loc] ~dir:`H [date; volume; issue; pages]
    in
    let edit_publisher = edit_publisher r in
    let authors = edit_contributor (Some Person.Author) uf authors in
    let editors = edit_contributor (Some Person.Editor) uf editors in
    El.details ~at:[open'; Hclass.field]
      [ summary; El.hr (); ref_ids; authors; editors; El.hr (); cites;
        container_details;
        container_title; edit_publisher;  ]
  in
  let subjects = edit_subjects uf subjects in
  let note = edit_note r in
  let private_note = edit_private_note r in
  let public = edit_public r in
  let buttons = edit_buttons uf ~submit r in
  Hfrag.form_no_submit
    [ title; details; El.hr (); subjects; note; private_note; public; buttons ]

let fill_ui g ~doi =
  let label =
    let at = [Hclass.font_small] in
    El.span ~at [El.txt Uimsg.fill_in_form_with_doi]
  in
  let input =
    let at = [At.placeholder "DOI"; At.required; At.type' "search"] in
    Hui.input_string' ~at ~autogrow:true ~min_size:25 ~name:"doi" doi
  in
  let fill_in =
    let tip = Uimsg.fill_in_form_with_doi in
    Hui.button ~type':"submit" ~tip (El.txt Uimsg.fill_in)
  in
  let input =
    let at = [Hclass.hspace_0_5] in
    Hui.group ~at ~x_align:`Center ~dir:`H [input; fill_in]
  in
  let at =
    let url = Reference.Url.v (Fill_in_form "") in
    let r = Hfrag.hc_request (Page.Gen.url_fmt g) url in
    let t = Hc.target ":up .entity" in
    [r; t; Hclass.vspace_0_125]
  in
  El.splice [El.form ~at [label; input]; El.hr ()]

let filled_in_form
    g ~self ~cancel r ~msg ~authors ~editors ~container ~cites
  =
  let form =
    edit_reference g ~self ~submit:(`New cancel) r
      ~authors ~editors ~subjects:[] ~container ~cites
  in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [msg; form]

let edit_form g r ~render_data:rs =
  let self = Reference.Url.page r in
  let authors = List.map (fun a -> `Exists a) (find_authors r rs) in
  let editors = List.map (fun e -> `Exists e) (find_editors r rs) in
  let subjects = find_subjects r rs in
  let container = Option.map (fun c -> `Exists c) (find_container r rs) in
  let h1 = h1_reference (Page.Gen.url_fmt g) ~self r in
  let form =
    edit_reference g ~self ~submit:`Edit r ~authors ~editors ~subjects
      ~container ~cites:[] (* Not exposed in the UI. *)
  in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [h1; form]

let new_form g r ~cancel =
  let self = Reference.Url.v (New_form { cancel }) in
  let title = Hfrag.title ~sub:Uimsg.new_reference ~sup:Uimsg.reference in
  let fill_ui = fill_ui g ~doi:"" in
  let form =
    let at = At.[Hclass.entity; Hclass.editing] in
    let authors = [] and editors = [] and subjects = [] in
    El.section ~at
      [ edit_reference g ~self ~submit:(`New cancel) r
          ~authors ~editors ~subjects ~container:None ~cites:[] ]
  in
  let content = El.section [fill_ui; form] in
  Page.with_content ?ui_ext:None g ~self ~title ~content

let view_title ~linkify uf ~self r =
  let title = El.txt_of Hfrag.sentencify (Reference.non_empty_title r) in
  let at = [Hclass.value; Hui.Class.for_col Reference.title'; viz r] in
  if not linkify then El.span ~at [title] else
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Reference.Url.page r) in
  Hfrag.link ~at ~href title

let undo_make_all_authors_public_button uf rid ~ids =
  let id i =
    El.input ~at:At.[hidden; name "undo"; value (string_of_int i)] ()
  in
  let ids = List.map id ids in
  let is_undo = El.input ~at:At.[hidden; name Hquery.is_undo; value "true"]() in
  let url = Reference.Url.v (Change_authors_publicity rid) in
  let label = Uimsg.undo_make_all_authors_public in
  Hfrag.hc_button ~query:":scope > *" ~target:Hfrag.target_entity uf url
    ~at:Hui.Class.[tiny] (El.splice (El.txt label :: is_undo :: ids))

let make_all_authors_public_button uf rid =
  let label = Uimsg.make_all_authors_public in
  let url = Reference.Url.v (Change_authors_publicity rid) in
  Hfrag.hc_button ~target:Hfrag.target_entity uf url
    ~at:Hui.Class.[tiny] (El.txt label)

let view_persons ?(ui = El.void) ~class' uf ~self = function
| [] -> El.void
| ps ->
    let persons = List.map (Hfrag.link_person uf ~self) ps in
    let persons = El.splice ~sep:(El.txt ", ") persons in
    El.splice [ El.span ~at:[class'] [persons; ui]]

let view_authors ?ui = view_persons ~class':Hclass.authors ?ui
let view_editors ?ui = view_persons ~class':Hclass.editors ?ui

let view_container_loc r =
  let vi = match Reference.(volume r, issue r) with
  | "", "" -> "" | n, "" | "", n -> n  | n, i -> Fmt.str "%s(%s)" n i
  in
  match vi, Reference.pages r with
  | "", "" -> ""
  | "", v | v, "" -> Fmt.str "%s" v
  | vi, pp -> Fmt.str "%s, %s" vi pp

let view_container ~details uf ~self r rd c =
  let loc = if not details then "" else view_container_loc r in
  let loc_at = At.[Hclass.container_loc; viz r] in
  match c, loc with
  | None, "" -> El.void
  | None, d -> El.span ~at:loc_at [El.txt_of (Fmt.str "%s. ") d]
  | Some c, loc  ->
      let eds = match Reference.is_monograph_part r with
      | false -> El.void
      | true ->
          let in' = El.txt (Fmt.str "%s " Uimsg.in') in
          let editors = find_editors r rd in
          let eds = view_editors uf ~self editors in
          let at = [Hclass.person; Hclass.value] (* Humpf… *) in
          match editors with
          | [] -> in'
          | [_] ->
              let abbr = El.txt (Fmt.str " (%s) " Uimsg.editor_abbr) in
              El.splice [in'; eds; El.span ~at [abbr]]
          | _ ->
              let abbr = El.txt (Fmt.str " (%s) " Uimsg.editors_abbr) in
              El.splice [in'; eds; El.span ~at [abbr]]
      in
      let title = match loc with
      | "" -> El.span ~at:[viz r] [Hfrag.link_container uf ~self c; El.txt ". "]
      | loc ->
          El.span ~at:[viz r]
            [ Hfrag.link_container uf ~self c;
              El.span ~at:loc_at [El.txt_of (Fmt.str ", %s. ") loc]]
      in
      El.splice [eds; title]

let view_publisher r = match Reference.publisher r with
| "" -> El.void | p -> El.txt_of (Fmt.str " %s. ") p

let view_year uf ~self r = match Reference.year r with
| 0 -> El.void
| n -> El.splice [Hfrag.link_year uf ~self (Reference.year r); El.sp]

let view_subjects uf ~self r = function
| [] -> El.void
| ss ->
    let ss = List.map (Hfrag.link_subject ~self uf) ss in
    El.p [El.splice ~sep:(El.txt ", ") ss]

let view_doi_link r = match Reference.doi r with
| "" -> El.void
| doi ->
    let dlink = Fmt.str "%s/%s" Doi.default_resolver doi in
    let at = At.[href dlink; v "data-doi" doi] in
    El.a ~at [El.txt Uimsg.full_text]

let view_cites_link uf ~self r =
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Reference.Url.page r) in
  let href = String.concat "#" [href; cites_anchor] in
  Hfrag.link ~href (El.txt Uimsg.cites)

let view_cited_by_link uf ~self r =
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Reference.Url.page r) in
  let href = String.concat "#" [href; cited_by_anchor] in
  Hfrag.link ~href (El.txt Uimsg.cited_by)

let note_html cl viz ~label ~in_details = function
| "" -> El.void
| note ->
  let note = El.p ~at:[Hclass.value; cl; viz] [El.txt note] in
  if not in_details then note else
  let label = El.summary [El.txt label] in
  El.details ~at:At.[viz] [label; note]

let view_note ~in_details r =
  let label = Uimsg.note in
  let cl = Hui.Class.for_col Reference.note' in
  note_html cl (viz r) ~in_details ~label (Reference.note r)

let view_private_note g ~in_details r =
  if not (Page.Gen.private_data g) then El.void else
  let viz = Hclass.private' and note = Reference.private_note r in
  let label = Uimsg.private_note in
  let cl = Hui.Class.for_col Reference.private_note' in
  note_html cl viz ~label ~in_details note

let view_more_details ~notes g ~self r =
  let uf = Page.Gen.url_fmt g in
  let notes = match notes with
  | false -> El.void
  | true ->
      let note = view_note ~in_details:true r in
      let private_note = view_private_note g ~in_details:true r in
      El.splice [note; private_note]
  in
  let full_text = view_doi_link r in
  let cites = view_cites_link uf ~self r in
  let cited_by = view_cited_by_link uf ~self r in
  El.div ~at:[Hclass.more_details] [full_text; cites; cited_by; notes]

let list_item g ~self r rs =
  let uf = Page.Gen.url_fmt g in
  let anchor_id = Fmt.str "%d" (Reference.id r) in
  let title = view_title ~linkify:true uf ~self r in
  let container = find_container r rs in
  let container = view_container ~details:false uf ~self r rs container in
  let year = view_year uf ~self r in
  let authors = find_authors r rs in
  let authors = view_authors uf ~self authors in
  let ref = El.p ~at:[Hclass.ref] [ title; El.sp; container; year; authors] in
  let subjects = find_subjects r rs in
  let subjects = view_subjects uf ~self r subjects in
  let more_details = view_more_details ~notes:true g ~self r in
  let at = At.[Hclass.ref_item; id anchor_id; viz r] in
  El.li ~at [Hfrag.anchor_a anchor_id; ref; subjects; more_details]

let list_by_desc_date uf ~self rs =
  let refs = List.sort (Fun.flip Reference.compare_by_date) rs.Reference.list in
  let list_item r = list_item uf ~self r rs in
  El.ol ~at:At.[Hclass.ref_list] (List.map list_item refs)

let list_section
    ?(anchor_id = Uimsg.references_anchor) ?title g ~self ~descr ~descr_zero rs
  =
  let title = match title with
  | None -> Hfrag.uppercase_span (Uimsg.references)
  | Some t -> t
  in
  let count = Hfrag.item_count (List.length rs.Reference.list) in
  let aid = anchor_id in
  let title = [Hfrag.anchor_a aid; title; El.sp; count] in
  let content = match List.length rs.Reference.list with
  | 0 -> Hfrag.description (El.txt descr_zero)
  | _ ->
      El.splice [Hfrag.description (El.txt descr);
                 list_by_desc_date g ~self rs]
  in
  El.section [El.h2 ~at:At.[id aid] title; content]

let edit_ui g uf r =
  if not (Page.Gen.editable g) then El.void else
  let rid = Reference.id r in
  let edit = Hfrag.hc_edit_button uf (Reference.Url.v (Edit_form rid)) in
  let del = Hfrag.hc_delete_button uf (Reference.Url.v (Confirm_delete rid)) in
  let left = Hui.group ~dir:`H [edit] in
  Hui.group ~at:[Hclass.entity_ui] ~align:`Justify ~dir:`H [left; del]

let view_fields ?authors_ui g ~self r ~render_data:rd =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_reference uf ~self r in
  let title = view_title ~linkify:false uf ~self r in
  let container = find_container r rd in
  let container = view_container ~details:false uf ~self r rd container in
  let author_list = find_authors r rd in
  let authors = view_authors uf ~self author_list in
  let authors_ui =
    if not (Page.Gen.editable g) then El.void else
    let ui = match authors_ui with
    | Some ui -> ui
    | None when List.for_all Person.public author_list -> El.void
    | None -> make_all_authors_public_button uf (Reference.id r)
    in
    El.splice [El.sp; El.nbsp; ui]
  in
  let year = view_year uf ~self r in
  let ref =
    El.p ~at:[Hclass.ref]
      [ title; El.sp; container; (* pub; *) year; authors; authors_ui]
  in
  let subjects = find_subjects r rd in
  let subjects = view_subjects uf ~self r subjects in
  let more_details = view_more_details ~notes:false g ~self r in
  let ref =
    El.div ~at:[Hclass.ref_item; viz r] [ref; subjects; more_details; ]
  in
  let note = view_note ~in_details:false r in
  let private_note = view_private_note ~in_details:false g r in
  let edit_ui = edit_ui g uf r in
  let editing = if Page.Gen.editable g then Hclass.editing else At.void in
  let at = At.[Hclass.entity; editing] in
  El.section ~at [ h1; ref; note; private_note; edit_ui]

let view_full g ~self r ~render_data ~cites ~cited_by =
  let cites =
    let descr = Uimsg.cites_descr in
    let descr_zero = Uimsg.cites_descr_zero in
    let anchor_id = cites_anchor in
    let title = Hfrag.uppercase_span Uimsg.cites in
    list_section g ~anchor_id ~title ~self ~descr ~descr_zero cites
  in
  let cited_by =
    let descr = Uimsg.cited_by_descr in
    let descr_zero = Uimsg.cited_by_descr_zero in
    let anchor_id = cited_by_anchor in
    let title = Hfrag.uppercase_span Uimsg.cited_by in
    list_section g ~anchor_id ~title ~self ~descr ~descr_zero cited_by
  in
  El.section [ view_fields g ~self r ~render_data; cites; cited_by ]

let deleted g s =
  let reference = Hfrag.uncapitalize Uimsg.reference in
  let goto = Reference.Url.v Reference.Url.Index in
  let goto = Kurl.Fmt.url (Page.Gen.url_fmt g) goto in
  let goto = Hfrag.link ~href:goto (El.txt_of Uimsg.goto_kind_index reference)in
  let msg = El.txt_of Uimsg.reference_deleted (Reference.title s) in
  El.section [ El.h1 [El.txt Uimsg.deleted]; El.p [msg]; El.p [goto]]

let page_title r = Hfrag.title ~sub:(Reference.title r) ~sup:Uimsg.reference
let page_full_title g r = Page.full_title g ~title:(page_title r)
let page g r ~render_data ~cites ~cited_by =
  let self = Reference.Url.page r in
  let title = page_title r in
  let content = view_full g ~self r ~render_data ~cites ~cited_by in
  Page.with_content ~ui_ext g ~self ~title ~content

let page_404 g ~self =
  let consult = Reference.Url.v Index in
  Page.for_404 ~ui_ext g ~kind:Uimsg.reference ~self ~consult

let index_html g ~self rs =
  let h1 =
    let count = Hfrag.item_count (List.length rs.Reference.list) in
    El.h1 [Hfrag.uppercase_span Uimsg.references; El.sp; count ]
  in
  let descr = Hfrag.description (El.txt Uimsg.reference_list_descr) in
  let index = list_by_desc_date g ~self rs in
  El.section [ h1; descr; index ]

let index g rs =
  let self = Kurl.v Reference.Url.kind Index in
  let content = index_html g ~self rs in
  Page.with_content ~ui_ext g ~self ~title:Uimsg.references ~content

(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern

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
