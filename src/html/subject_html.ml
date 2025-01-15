(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let viz = Entity_html.viz (module Subject)

let ui_ext g ~self =
  if not (Page.Gen.editable g) then El.void else
  let new_button =
    let uf = Page.Gen.url_fmt g in
    let cancel = Some (Kurl.Fmt.url uf self) in
    let dst = Subject.Url.v (New_form { cancel }) in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
    Html_kit.new_entity_button ~href ~label:Uimsg.new_subject
  in
  Hui.group ~at:At.[Hclass.entity_menu] ~dir:`H [new_button]

let entity_cancel_button uf s =
  let cancel = Subject.Url.v (View_fields (Subject.id s)) in
  Html_kit.htmlact_cancel_button uf cancel

let h1_subject uf ~self ?name s =
  let entity_kind =
    let kind = Uimsg.subjects in
    Html_kit.entity_kind_index uf ~self ~kind (Subject.Url.v Index)
  in
  let name = match name with
  | Some name -> name s
  | None ->
      let at = [viz s; Hclass.subject; Hclass.value] in
      El.span ~at [El.txt_of Subject.name s]
  in
  El.h1 [entity_kind; El.sp; name]

let confirm_delete g s ~ref_count =
  (* TODO also warn about existing children and what happens to them. *)
  let self = Subject.Url.page s in
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_subject uf ~self s in
  let cancel_button = entity_cancel_button uf s in
  let delete_button =
    let confirm = Subject.Url.v (Delete (Subject.id s)) in
    let target = Html_kit.target_entity_up in
    Html_kit.htmlact_delete uf confirm ~target (El.txt Uimsg.confirm_delete)
  in
  let bs = Hui.group ~align:`Justify ~dir:`H [delete_button; cancel_button] in
  let really = El.p [El.txt_of Uimsg.really_delete_subject (Subject.name s)] in
  let used = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Html_kit.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.message; Hclass.info] in
      El.p ~at [El.txt Uimsg.it_is_still_applied_to; El.sp; refs; El.txt "."]
  in
  let no_undo_warn =
    El.p ~at:[Hclass.message; Hclass.warn] [El.txt Uimsg.this_cannot_be_undone]
  in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [ h1; really; used; no_undo_warn; bs; ]

let edit_name s =
  let label = El.txt Uimsg.name in
  Hui.field_string ~autogrow:true ~min_size:5 ~label ~col:Subject.name' s

let edit_description = Entity_html.edit_description (module Subject)
let edit_private_note = Entity_html.edit_private_note (module Subject)
let edit_public = Entity_html.edit_public (module Subject)
let edit_parent s ~parents =
  let label = El.txt Uimsg.parent in
  let parents = List.sort Subject.order_by_name parents in
  let options = None :: List.map (fun s -> Some (Subject.id s)) parents in
  let parents = Subject.Id.Map.of_elts Subject.id parents in
  let option_text = function
  | None -> Uimsg.no_parent
  | Some id -> Subject.name (Subject.Id.Map.find id parents)
  in
  let option_value = function None -> "" | Some id -> Subject.Id.to_string id in
  let col = Subject.parent' in
  let select_at = At.[Hclass.subject; Hclass.value] in
  Hui.field_select ~select_at ~label ~option_text ~option_value ~options ~col s

let edit_submit uf ~submit s =
  let url, label = match submit with
  | `New _ -> Subject.Url.v Create, Uimsg.create_subject
  | `Edit -> Subject.Url.v (Update (Subject.id s)), Uimsg.save_subject
  | `Duplicate ->
      Subject.Url.v (Duplicate (Subject.id s)), Uimsg.create_duplicate
  in
  let r = Html_kit.htmlact_request uf url and e = Htmlact.effect' `Element in
  let q = Htmlact.query "form:up" in
  let rescue = Htmlact.query_rescue (`Bool true) in
  let t = Htmlact.target ":up :up" in
  let at = At.[t; r; e; q; rescue; Hui.Class.submit] in
  Hui.button ~at (El.txt label)

let edit_cancel uf ~submit s = match submit with
| `Edit | `New None | `Duplicate -> entity_cancel_button uf s
| `New (Some cancel_href) ->
    Hui.button_link ~href:cancel_href (El.txt Uimsg.cancel)

let edit_buttons uf ~submit s =
  let cancel = edit_cancel uf ~submit s in
  let submit = edit_submit uf ~submit s in
  Hui.group ~align:`Justify ~dir:`H [cancel; submit]

let edit_subject ?(msg = El.void) g ~self ~submit s ~parents =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_subject uf ~self ~name:edit_name s in
  let parent = edit_parent s ~parents in
  let description = edit_description s in
  let private_note = edit_private_note s in
  let public = edit_public s in
  let buttons = edit_buttons uf ~submit s in
  Html_kit.entity_form_no_submit
    [h1; parent; description; private_note; public; msg; buttons]

let edit_form g s ~parents =
  let self = Subject.Url.page s in
  edit_subject g ~self ~submit:`Edit s ~parents

let duplicate_form g s ~ref_count ~parents =
  let self = Subject.Url.page s in
  let msg = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Html_kit.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.message; Hclass.info] in
      El.p ~at [El.txt Uimsg.subject_duplicate_will_be_applied_to; El.sp;
                refs; El.txt "."]
  in
  edit_subject g ~self ~submit:`Duplicate s ~parents ~msg

let new_form g s ~parents ~cancel =
  let self = Subject.Url.v (New_form { cancel }) in
  let title = Html_kit.title ~sub:Uimsg.new_subject ~sup:Uimsg.subject in
  let content =
    let at = At.[Hclass.entity; Hclass.editing] in
    El.section ~at [ edit_subject g ~self ~submit:(`New cancel) s ~parents ]
  in
  Page.with_content ?ui_ext:None g ~self ~title ~content

let replace_form g s ~ref_count =
  let self = Subject.Url.page s in
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_subject uf ~self s in
  let buttons =
    let cancel = entity_cancel_button uf s in
    let submit = Hui.submit (El.txt Uimsg.replace_subject) in
    Hui.group ~align:`Justify ~dir:`H [cancel; submit]
  in
  let intro = El.p [El.txt_of Uimsg.replace_subject_by (Subject.name s)] in
  let input_subject =
    let for_list = false and input_name = Entity.Url.replace_by in
    Entity_html.subject_input_finder uf ~for_list ~input_name
  in
  let msg = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Html_kit.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.message; Hclass.info] in
      El.p ~at [El.txt Uimsg.replacement_subject_will_be_applied_to; El.sp;
                refs; El.txt "."]
  in
  let at =
    let r = Html_kit.htmlact_request uf (Subject.Url.v (Replace (Subject.id s))) in
    let e = Htmlact.effect' `Element in
    At.[Hclass.entity; Hclass.editing; r; e]
  in
  let replace = El.div ~at:[Hclass.replace] [input_subject] in
  El.form ~at [h1; intro; replace; msg; buttons]

let view_parent uf ~self = function
| None -> El.void
| Some p ->
    let link = Html_kit.link_subject uf ~self p in
    let at = [Hclass.value; Hui.Class.for_col Subject.parent'] in
    El.p ~at [El.txt Uimsg.parent; El.sp; link]

let view_description = Entity_html.view_description (module Subject)
let view_private_note = Entity_html.view_private_note (module Subject)

let edit_ui g uf s =
  if not (Page.Gen.editable g) then El.void else
  let sid = Subject.id s in
  let edit = Html_kit.htmlact_edit_button uf (Subject.Url.v (Edit_form sid)) in
  let rep =
    Html_kit.htmlact_replace_button uf (Subject.Url.v (Replace_form sid))
  in
  let dup =
    Html_kit.htmlact_duplicate_button uf (Subject.Url.v (Duplicate_form sid))
  in
  let del =
    Html_kit.htmlact_delete_button uf (Subject.Url.v (Confirm_delete sid))
  in
  let left = Hui.group ~dir:`H [edit; rep; dup] in
  Hui.group ~at:[Hclass.entity_ui] ~align:`Justify ~dir:`H [left; del]

let view_fields g ~self s ~parent =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_subject uf ~self s in
  let parent = view_parent uf ~self parent in
  let description = view_description s in
  let private_note = view_private_note g s in
  let edit_ui = edit_ui g uf s in
  let editing = if Page.Gen.editable g then Hclass.editing else At.void in
  let at = At.[Hclass.entity; editing] in
  El.section ~at [h1; parent; description; private_note; edit_ui]

let view_full g ~self s ~parent refs =
  let refs =
    let descr = Uimsg.subject_applied_descr (Subject.name s) in
    let descr_zero = Uimsg.subject_applied_descr_zero (Subject.name s) in
    Reference_html.list_section g ~self ~descr ~descr_zero refs
  in
  El.section [ view_fields g s ~self ~parent; refs ]

let deleted g s =
  let subject = Html_kit.uncapitalize Uimsg.subject in
  let goto = Subject.Url.v Subject.Url.Index in
  let goto = Kurl.Fmt.url (Page.Gen.url_fmt g) goto in
  let goto = Html_kit.link ~href:goto (El.txt_of Uimsg.goto_kind_index subject) in
  let msg = El.txt_of Uimsg.subject_deleted (Subject.name s) in
  El.section [ El.h1 [El.txt Uimsg.deleted]; El.p [msg]; El.p [goto]]

let page_title s = Html_kit.title ~sub:(Subject.name s) ~sup:Uimsg.subject
let page_full_title g s = Page.full_title g ~title:(page_title s)
let page g s ~parent refs =
  let self = Subject.Url.page s in
  let title = page_title s in
  let content = view_full g ~self s ~parent refs in
  Page.with_content ~ui_ext g ~self ~title ~content

let page_404 g ~self =
  let consult = Subject.Url.v Subject.Url.Index in
  Page.for_404 ~ui_ext g ~kind:Uimsg.subject ~self ~consult

let index_html g ~self ss ~ref_count =
  let uf = Page.Gen.url_fmt g in
  let anchor_id s = Subject.Id.to_string (Subject.id s) in
  let subject s = Html_kit.link_subject uf ~self s in
  let ref_count s = match Subject.Id.Map.find_opt (Subject.id s) ref_count with
  | None -> 0 | Some (_, c) -> c
  in
  let child s =
    let sid = anchor_id s in
    let count = Html_kit.item_count (ref_count s) in
    El.li ~at:At.[id sid] [Html_kit.anchor_a sid; subject s; El.sp; count]
  in
  let parent children r =
    let h2 =
      let sid = anchor_id r in
      let count = Html_kit.item_count (ref_count r) in
      El.h2 ~at:At.[id sid] [Html_kit.anchor_a sid; subject r; El.sp; count]
    in
    let children = match Subject.Id.Map.find_opt (Subject.id r) children with
    | None | Some [] -> El.void
    | Some children ->
        let children = List.sort Subject.order_by_name children in
        El.ol ~at:[viz r] (List.map child children)
    in
    let subject_descr = view_description r in
    El.splice [ h2; subject_descr; children]
  in
  let h1 =
    let count = Html_kit.item_count (List.length ss) in
    El.h1 [Html_kit.uppercase_span Uimsg.subjects; El.sp; count ]
  in
  let tree =
    let parents, children = Subject.hierarchy ss in
    let parents = List.sort Subject.order_by_name parents in
    let at = [Hclass.index; Hclass.subject] in
    El.nav ~at (List.map (parent children) parents)
  in
  El.section [h1; tree]

let index g ss ~ref_count =
  let self = Subject.Url.v Index in
  let content = index_html g ~self ss ~ref_count in
  Page.with_content ~ui_ext g ~self ~title:Uimsg.subjects ~content
