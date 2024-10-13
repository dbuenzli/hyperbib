(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let ui_ext g ~self =
  if not (Page.Gen.editable g) then El.void else
  let new_button =
    let uf = Page.Gen.url_fmt g in
    let cancel = Some (Kurl.Fmt.url uf self) in
    let dst = Container.Url.v (New_form { cancel }) in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
    Html_kit.new_entity_button ~href ~label:Uimsg.new_container
  in
  Hui.group ~at:At.[Hclass.entity_menu] ~dir:`H [new_button]

let entity_cancel_button uf c =
  let cancel = Container.Url.v (View_fields (Container.id c)) in
  Html_kit.htmlact_cancel_button uf cancel

let h1_container ?title uf ~self c =
  let viz = if Container.public c then At.void else Hclass.private' in
  let entity_kind =
    let kind = Uimsg.containers in
    Html_kit.entity_kind_index uf ~self ~kind (Container.Url.v Index)
  in
  let title = match title with
  | None -> El.span ~at:[viz; Hclass.container] [El.txt_of Container.title c]
  | Some names -> names c
  in
  El.h1 [entity_kind; El.sp; title]

let confirm_delete g c ~ref_count =
  let self = Container.Url.page c in
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_container uf ~self c in
  let cancel_button = entity_cancel_button uf c in
  let delete_button =
    let confirm = Container.Url.v (Delete (Container.id c)) in
    let target = Html_kit.target_entity_up in
    Html_kit.htmlact_delete uf confirm ~target (El.txt Uimsg.confirm_delete)
  in
  let bs = Hui.group ~align:`Justify ~dir:`H [delete_button; cancel_button] in
  let really =
    El.p [ El.txt_of Uimsg.really_delete_container (Container.title c) ]
  in
  let used = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Html_kit.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.message; Hclass.error] in
      El.p ~at [El.txt Uimsg.this_will_also_delete; El.sp; refs; El.txt "."]
  in
  let no_undo_warn =
    El.p ~at:[Hclass.message; Hclass.warn] [El.txt Uimsg.this_cannot_be_undone]
  in
  let at = At.[Hclass.entity; Hclass.editing] in
  El.section ~at [ h1; really; used; no_undo_warn;  bs; ]

let deleted g c =
  let cont = Html_kit.uncapitalize Uimsg.container in
  let goto = Container.Url.v Index in
  let goto = Kurl.Fmt.url (Page.Gen.url_fmt g) goto in
  let goto = Html_kit.link ~href:goto (El.txt_of Uimsg.goto_kind_index cont) in
  let msg = El.txt_of Uimsg.container_deleted (Container.title c) in
  El.section [ El.h1 [El.txt Uimsg.deleted]; El.p [msg]; El.p [goto]]

let edit_title c =
  let label = El.txt Uimsg.title in
  Hui.field_text
    ~autogrow:true ~min_rows:1 ~label ~col:Container.title' c

let edit_issn c =
  let label = El.txt Uimsg.issn in
  Hui.field_string ~autogrow:true ~min_size:10 ~label ~col:Container.issn' c

let edit_isbn c =
  let label = El.txt Uimsg.isbn in
  Hui.field_string ~autogrow:true ~min_size:15 ~label ~col:Container.isbn' c

let edit_note = Entity_html.edit_note (module Container)
let edit_private_note = Entity_html.edit_private_note (module Container)
let edit_public = Entity_html.edit_public (module Container)

let edit_submit uf ~submit c =
  let url, label = match submit with
  | `New _ -> Container.Url.v Create, Uimsg.create_container
  | `Edit -> Container.Url.v (Update (Container.id c)), Uimsg.save_container
  | `Duplicate ->
      Container.Url.v (Duplicate (Container.id c)), Uimsg.create_duplicate
  in
  let r = Html_kit.htmlact_request uf url and e = Htmlact.effect `Element in
  let q = Htmlact.query "form:up" in
  let rescue = Htmlact.query_rescue (`Bool true) in
  let t = Htmlact.target ":up :up :up" in
  let at = At.[t; r; e; q; rescue; Hui.Class.submit] in
  Hui.button ~at (El.txt label)

let edit_cancel uf ~submit p = match submit with
| `Edit | `New None | `Duplicate -> entity_cancel_button uf p
| `New (Some cancel_href) ->
    Hui.button_link ~href:cancel_href (El.txt Uimsg.cancel)

let edit_buttons uf ~submit c =
  let cancel = edit_cancel uf ~submit c in
  let submit = edit_submit uf ~submit c in
  Hui.group ~align:`Justify ~dir:`H [cancel; submit]

let edit_container ?(msg = El.void) g ~self ~submit c =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_container uf ~self ~title:(Fun.const El.void) c in
  let title = edit_title c in
  let ids =
    let issn = edit_issn c in
    let isbn = edit_isbn c in
    Hui.group ~at:[Hclass.container_ids] ~dir:`H [issn; isbn]
  in
  let note = edit_note c in
  let private_note = edit_private_note c in
  let public = edit_public c in
  let buttons = edit_buttons uf ~submit c in
  Html_kit.entity_form_no_submit
    [h1; title; ids; note; private_note; public; msg; buttons]

let edit_form g c =
  let self = Container.Url.page c in
  edit_container  g ~self ~submit:`Edit c

let new_form g c ~cancel =
  let self = Container.Url.v (New_form { cancel }) in
  let title = Html_kit.title ~sub:Uimsg.new_container ~sup:Uimsg.container in
  let content =
    let at = At.[Hclass.entity; Hclass.editing] in
    El.section ~at [edit_container g ~self ~submit:(`New cancel) c]
  in
  Page.with_content ?ui_ext:None g ~self ~title ~content

let duplicate_form g c =
  let self = Container.Url.page c in
  edit_container g ~self ~submit:`Duplicate c

let replace_form g c ~ref_count =
  let self = Container.Url.page c in
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_container uf ~self c in
  let buttons =
    let cancel = entity_cancel_button uf c in
    let submit = Hui.submit (El.txt Uimsg.replace_container) in
    Hui.group ~align:`Justify ~dir:`H [cancel; submit]
  in
  let intro =
    let intro = Uimsg.replace_container_by (Container.title c) in
    El.p [El.txt intro]
  in
  let input_name = Entity.Url.replace_by in
  let input_container = Entity_html.container_input_finder uf ~input_name in
  let msg = match ref_count with
  | 0 -> El.void
  | n ->
      let href = Html_kit.anchor_href Uimsg.references_anchor in
      let refs = El.a ~at:[href] [El.txt_of Uimsg.these_n_references n] in
      let at = [Hclass.message; Hclass.info] in
      El.p ~at [El.txt Uimsg.replacement_container_will_become_container_of;
                El.sp; refs; El.txt "."]
  in
  let at =
    let r =
      Html_kit.htmlact_request uf (Container.Url.v (Replace (Container.id c)))
    in
    let e = Htmlact.effect `Element in
    At.[Hclass.entity; Hclass.editing; r; e]
  in
  let replace = El.div ~at:[Hclass.replace] [input_container]in
  El.form ~at [h1; intro; replace; msg; buttons]

let view_note = Entity_html.view_note (module Container)
let view_private_note = Entity_html.view_private_note (module Container)

let edit_ui g uf c =
  if not (Page.Gen.editable g) then El.void else
  let cid = Container.id c in
  let edit =
    Html_kit.htmlact_edit_button uf (Container.Url.v (Edit_form cid)) in
  let rep =
    Html_kit.htmlact_replace_button uf (Container.Url.v (Replace_form cid)) in
  let dup =
    Html_kit.htmlact_duplicate_button uf (Container.Url.v (Duplicate_form cid))
  in
  let del =
    Html_kit.htmlact_delete_button uf (Container.Url.v (Confirm_delete cid))
  in
  let left = Hui.group ~dir:`H [edit; rep; dup] in
  Hui.group ~at:[Hclass.entity_ui] ~align:`Justify ~dir:`H [left; del]

let view_fields g ~self c =
  let uf = Page.Gen.url_fmt g in
  let h1 = h1_container uf ~self c in
  let note = view_note c in
  let private_note = view_private_note g c in
  let edit_ui = edit_ui g uf c in
  let editing = if Page.Gen.editable g then Hclass.editing else At.void in
  let at = At.[Hclass.entity; editing] in
  El.section ~at [h1; note; private_note; edit_ui; ]

let view_full g ~self c refs =
  let refs =
    let descr = Uimsg.container_contained (Container.title c) in
    let descr_zero = Uimsg.container_contained_zero (Container.title c) in
    Reference_html.list_section g ~self ~descr ~descr_zero refs
  in
  El.section [ view_fields g ~self c; refs ]

let page_404 g ~self =
  let consult = Container.Url.v Index in
  Page.for_404 ~ui_ext g ~kind:Uimsg.container ~self ~consult

let page_title c = Html_kit.title ~sub:(Container.title c) ~sup:Uimsg.container
let page_full_title g s = Page.full_title g ~title:(page_title s)
let page g c refs =
  let self = Container.Url.page c in
  let title = page_title c in
  let content = view_full g ~self c refs in
  Page.with_content ~ui_ext g ~self ~title ~content

let index_html g ~self cs ~ref_count =
  let uf = Page.Gen.url_fmt g in
  let ref_count c =
    match Container.Id.Map.find_opt (Container.id c) ref_count with
    | None -> 0 | Some (_, c) -> c
  in
  let container_li p =
    let cid = Container.Id.to_string (Container.id p) in
    let cont = Html_kit.link_container uf ~self p in
    let count = Html_kit.item_count (ref_count p) in
    let container = [Html_kit.anchor_a cid; cont; El.sp; count] in
    El.li ~at:At.[id cid] container
  in
  let letter_section (l, cs) =
    let cs = List.sort Container.order_by_title cs in
    El.splice [Html_kit.h2_letter l; El.ol (List.map container_li cs)]
  in
  let h1 =
    let count = Html_kit.item_count (List.length cs) in
    El.h1 [Html_kit.uppercase_span Uimsg.containers; El.sp; count ]
  in
  let descr = Html_kit.description (El.txt Uimsg.container_list_descr) in
  let index =
    let classes p = match Container.index_letter p with
    | None -> ["\u{2300}"] | Some c -> [String.of_char c]
    in
    let letters = List.classify ~classes cs in
    let letters_nav = Html_kit.letters_nav (List.map fst letters) in
    let letter_sections = El.splice (List.map letter_section letters) in
    let at = [Hclass.container; Hclass.index ] in
    El.nav ~at [letters_nav; letter_sections]
  in
  El.section [h1; descr; index]

let index g cs ~ref_count =
  let self = Container.Url.v Index in
  let content = index_html g ~self cs ~ref_count in
  Page.with_content ~ui_ext g ~self ~title:Uimsg.containers ~content
