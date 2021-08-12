(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let ui_ext g ~self =
  if not (Page.Gen.editable g) then El.void else
  let new_button =
    let uf = Page.Gen.url_fmt g in
    let cancel = Some (Kurl.Fmt.url uf self) in
    let dst = Person.Url.v (New_form { cancel }) in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
    Hfrag.new_entity_button ~href ~label:Uimsg.new_person
  in
  Hui.group ~at:At.[Hclass.entity_menu] ~dir:`H [new_button]

let entity_cancel_button uf p =
  let cancel = Person.Url.v (View_fields (Person.id p)) in
  Hfrag.hc_cancel_button uf cancel

let orcid_html p = match Person.orcid p with
| "" -> El.void
| href ->
    let viz = if Person.public p then At.void else Hclass.private' in
    let cl = Hui.Class.for_col Person.orcid' in
    Hfrag.link ~at:[Hclass.value; cl; viz] ~href (El.txt Uimsg.orcid)

let h1_person uf ~self ?names ~orcid p =
  let viz = if Person.public p then At.void else Hclass.private' in
  let entity_kind =
    let kind = Uimsg.persons in
    Hfrag.entity_kind_index uf ~self ~kind (Person.Url.v Index)
  in
  let names = match names with
  | None -> El.span ~at:[viz; Hclass.person] [El.txt_of Person.names_fl p]
  | Some names -> names p
  in
  let orcid = if orcid then El.splice [El.sp; orcid_html p] else El.void in
  El.h1 [entity_kind; El.sp; names; orcid]

let confirm_delete g p ~ref_count =
  let self = Person.Url.page p in
  let uf = Page.Gen.url_fmt g in
  let cancel_button = entity_cancel_button uf p in
  let delete_button =
    let confirm = Person.Url.v (Delete (Person.id p)) in
    let target = Hfrag.target_entity_up in
    Hfrag.hc_delete uf confirm ~target (El.txt Uimsg.confirm_delete)
  in
  let bs = Hui.group ~align:`Justify ~dir:`H [delete_button; cancel_button] in
  let msg =
    let used_warn = match ref_count with
    | 0 -> El.void
    | n ->
        let href = Hfrag.anchor_href Uimsg.references_anchor in
        let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
        El.splice [El.txt Uimsg.it_will_be_removed_from; El.sp; refs;
                   El.txt "."]
    in
    let really = El.txt_of Uimsg.really_delete_person (Person.names_fl p) in
    El.p [really; El.sp; used_warn]
  in
  let no_undo_warn = El.p [El.txt Uimsg.this_cannot_be_undone] in
  let ui_msg = El.div ~at:[Hclass.ui_msg] [msg; no_undo_warn] in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [ h1_person uf ~self ~orcid:true p; ui_msg; bs; ]

let deleted g p =
  let person = Hfrag.uncapitalize Uimsg.person in
  let goto = Person.Url.v Index in
  let goto = Kurl.Fmt.url (Page.Gen.url_fmt g) goto in
  let goto = Hfrag.link ~href:goto (El.txt_of Uimsg.goto_kind_index person) in
  let msg = El.txt_of Uimsg.person_deleted (Person.names_fl p) in
  El.section [ El.h1 [El.txt Uimsg.deleted]; El.p [msg]; El.p [goto]]

let edit_names p =
  let min_size = 5 and autogrow = true in
  let lst =
    let label = El.txt Uimsg.last_name in
    Hui.field_string ~autogrow ~min_size ~label ~col:Person.last_name' p
  in
  let fst =
    let label = El.txt Uimsg.first_names in
    Hui.field_string ~autogrow ~min_size ~label ~col:Person.first_names' p
  in
  El.splice [fst; El.sp; lst ]

let edit_orcid p =
  let label = El.txt Uimsg.orcid in
  Hui.field_string ~autogrow:true ~min_size:20 ~label ~col:Person.orcid' p

let edit_note = Entity_html.edit_note (module Person)
let edit_private_note = Entity_html.edit_private_note (module Person)
let edit_public = Entity_html.edit_public (module Person)

let edit_submit uf ~submit p =
  let url, label = match submit with
  | `New _ -> Person.Url.v Create, Uimsg.create_person
  | `Edit -> Person.Url.v (Update (Person.id p)), Uimsg.save_person
  | `Duplicate -> Person.Url.v (Duplicate (Person.id p)), Uimsg.create_duplicate
  in
  let r = Hfrag.hc_request uf url and e = Hc.effect `Element in
  let q = Hc.query "form:up" in
  let t = Hc.target ":up :up :up" in
  let at = At.[t; r; e; q; Hui.Class.submit] in
  Hui.button ~at (El.txt label)

let edit_cancel uf ~submit p = match submit with
| `Edit | `New None | `Duplicate -> entity_cancel_button uf p
| `New (Some cancel_href) ->
    Hui.button_link ~href:cancel_href (El.txt Uimsg.cancel)

let edit_buttons uf ~submit p =
  let cancel = edit_cancel uf ~submit p in
  let submit = edit_submit uf ~submit p in
  Hui.group ~align:`Justify ~dir:`H [cancel; submit]

let edit_person ?(msg = El.void) g ~self ~submit p =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_person uf ~self ~orcid:false ~names:edit_names p in
  let orcid = edit_orcid p in
  let note = edit_note p in
  let private_note = edit_private_note p in
  let public = edit_public p in
  let buttons = edit_buttons uf ~submit p in
  Hfrag.entity_form_no_submit
    [h1; orcid; note; private_note; public; msg; buttons]

let edit_form g p =
  let self = Person.Url.page p in
  edit_person g ~self ~submit:`Edit p

let new_form g p ~cancel =
  let self = Person.Url.v (New_form { cancel }) in
  let title = Hfrag.title ~sub:Uimsg.new_person ~sup:Uimsg.person in
  let content =
    let at = At.[Hclass.entity; Hclass.editing] in
    El.section ~at [ edit_person g ~self ~submit:(`New cancel) p ]
  in
  Page.html ?ui_ext:None g ~self ~title ~content

let duplicate_form g p ~ref_count =
  let self = Person.Url.page p in
  let msg = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Hfrag.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.ui_msg] in
      El.p ~at [El.txt Uimsg.person_duplicate_will_be_added_to; El.sp;
                refs; El.txt "."]
  in
  edit_person g ~self p ~submit:`Duplicate ~msg

let input_person ~name ~persons =
  (* XXX this doesn't scale. *)
  let persons = List.sort Person.order_by_last_name persons in
  let option p =
    let at = [At.value (Res.Id.to_string (Person.id p))] in
    El.option ~at [El.txt_of Person.names_lf p]
  in
  let at =
    [Hclass.person; Hui.Class.input; Hclass.select; At.required; At.name name]
  in
  El.select ~at (List.map option persons)

let replace_form g p ~ref_count ~persons =
  let self = Person.Url.page p in
  let uf = Page.Gen.url_fmt g in
  let buttons =
    let cancel = entity_cancel_button uf p in
    let submit = Hui.submit (El.txt Uimsg.replace_person) in
    Hui.group ~align:`Justify ~dir:`H [cancel; submit]
  in
  let intro =
    let intro = Uimsg.replace_person_by (Person.names_fl p) in
    El.p ~at:[Hclass.ui_msg] [El.txt intro]
  in
  let input_person = input_person ~name:Entity.Url.replace_by ~persons in
  let msg = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Hfrag.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.ui_msg] in
      El.p ~at [El.txt Uimsg.replacement_person_will_be_added_to; El.sp;
                refs; El.txt "."]
  in
  let at =
    let r = Hfrag.hc_request uf (Person.Url.v (Replace (Person.id p))) in
    let e = Hc.effect `Element in
    At.[Hclass.entity; Hclass.editing; r; e]
  in
  let h1 = h1_person uf ~self ~orcid:true p in
  El.form ~at [h1; intro; El.div [input_person]; msg; buttons]

let view_note = Entity_html.view_note (module Person)
let view_private_note = Entity_html.view_private_note (module Person)

let edit_ui g uf s =
  if not (Page.Gen.editable g) then El.void else
  let pid = Person.id s in
  let edit = Hfrag.hc_edit_button uf (Person.Url.v (Edit_form pid)) in
  let rep = Hfrag.hc_replace_button uf (Person.Url.v (Replace_form pid)) in
  let dup = Hfrag.hc_duplicate_button uf (Person.Url.v (Duplicate_form pid)) in
  let del = Hfrag.hc_delete_button uf (Person.Url.v (Confirm_delete pid)) in
  let left = Hui.group ~dir:`H [edit; rep; dup] in
  Hui.group ~at:[Hclass.entity_ui] ~align:`Justify ~dir:`H [left; del]

let view_fields g ~self p =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_person uf ~self ~orcid:true p in
  let note = view_note p in
  let private_note = view_private_note g p in
  let edit_ui = edit_ui g uf p in
  let editing = if Page.Gen.editable g then Hclass.editing else At.void in
  let at = At.[Hclass.entity; editing] in
  El.section ~at [h1; note; private_note; edit_ui; ]

let view_full g ~self s refs =
  let refs =
    let descr = Uimsg.person_mentioned_descr (Person.names_fl s) in
    let descr_zero = Uimsg.person_mentioned_descr_zero (Person.names_fl s) in
    Reference_html.list_section g ~self ~descr ~descr_zero refs
  in
  El.section [ view_fields g ~self s; refs ]

let page_404 g ~self =
  let consult = Person.Url.v Index in
  Page.html_404 ~ui_ext g ~kind:Uimsg.person ~self ~consult

let page_title s = Hfrag.title ~sub:(Person.names_fl s) ~sup:Uimsg.person
let page_full_title g s = Page.full_title g (page_title s)
let page g p refs =
  let self = Person.Url.page p in
  let title = page_title p in
  let content = view_full g ~self p refs in
  Page.html ~ui_ext g ~self ~title ~content

let index_html g ~self ps ~ref_count =
  let uf = Page.Gen.url_fmt g in
  let ref_count p = match Id.Map.find_opt (Person.id p) ref_count with
  | None -> 0 | Some (_, c) -> c
  in
  let person_li p =
    let pid = Fmt.str "%d" (Person.id p) in
    let names = Hfrag.link_person uf ~self p in
    let count = Hfrag.item_count (ref_count p) and orcid = orcid_html p in
    let person = [Hfrag.anchor_a pid; names; El.sp; count; El.sp; orcid] in
    El.li ~at:At.[id pid] person
  in
  let letter_section (l, ps) =
    let ps = List.sort Person.order_by_last_name ps in
    El.splice [Hfrag.h2_letter l; El.ol (List.map person_li ps)]
  in
  let h1 =
    let count = Hfrag.item_count (List.length ps) in
    El.h1 [Hfrag.uppercase_span Uimsg.persons; El.sp; count ]
  in
  let descr = Hfrag.description (El.txt Uimsg.person_list_descr) in
  let index =
    let classes p = match Person.index_letter p with
    | None -> ["\u{2300}"] | Some c -> [String.of_char c]
    in
    let letters = List.classify ~classes ps in
    let letters_nav = Hfrag.letters_nav (List.map fst letters) in
    let letter_sections = El.splice (List.map letter_section letters) in
    El.nav ~at:At.[Hclass.index; Hclass.person] [letters_nav; letter_sections]
  in
  El.section [h1; descr; index]

let index g ps ~ref_count =
  let self = Person.Url.v Index in
  let content = index_html g ~self ps ~ref_count in
  Page.html ~ui_ext g ~self ~title:Uimsg.persons ~content

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
