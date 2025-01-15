(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let page_404 g ~self =
  let consult = Suggestion.Url.v Index in
  Page.for_404 g ~kind:Uimsg.suggestion ~self ~consult

let doi_input g ?invalid_user_doi s =
  let label = El.txt (String.concat " " Uimsg.[doi; optional]) in
  let label = El.span ~at:[Hui.Class.label] [label] in
  let col = Suggestion.doi' in
  let input =
    (* TODO Hui support for coded columns and restoring invalid inputs *)
    let name = Rel.Col.name col in
    let v = match invalid_user_doi with
    | Some doi -> doi
    | None ->
        match Rel.Col.proj col s with
        | None -> "" | Some doi -> Doi.to_string doi
    in
    Hui.input_string' ~autogrow:true ~min_size:25 ~name v
  in
  let fill_in =
    let r =
      Html_kit.htmlact_request (Page.Gen.url_fmt g) (Suggestion.Url.v Fill_in)
    in
    let t = Htmlact.target "form:up" in
    let q = Htmlact.query "form:up" in
    let tip = Uimsg.fill_in_suggestion_with_doi in
    let text = El.txt Uimsg.fill_in_suggestion in
    Hui.button ~type':"submit" ~at:[r;t;q;] ~tip text
  in
  let input =
    let at = [Hclass.Gap.v_050] in
    Hui.group ~at ~x_align:`Center ~dir:`H [input; fill_in]
  in
  let col_class = Hui.Class.for_col col in
  let text = [label; El.sp; input] in
  El.label ~at:(Hclass.field :: col_class :: [Hclass.vspace_000]) text

let input_suggestion s =
  let label = El.txt Uimsg.suggested_reference in
  let col = Suggestion.suggestion' in
  Hui.field_text ~autogrow:true ~min_rows:3 ~label ~col s

let input_comment s =
  let comment = String.lowercase_ascii Uimsg.comment in
  let private_comment = [Uimsg.private'; comment; Uimsg.optional] in
  let label = El.txt (String.concat " " private_comment) in
  let col = Suggestion.comment' in
  Hui.field_text ~autogrow:true ~min_rows:2 ~label ~col s

let email_field = "suggester" (* The bot honeypot uses "email" *)
let input_email s =
  let label = El.txt (String.concat " " Uimsg.[your_email; email_note]) in
  let name = email_field and data = Suggestion.email s in
  Hui.field_string' ~autogrow:true ~min_size:30 ~label ~name data

let bot_honeypot_field = "email"
let bot_honeypot =
  let label = El.txt Uimsg.email in
  let min_size = 30 in
  let at = At.[class' "email"] in
  let input_at = At.[type' "search"] (* avoid autocomplete *) in
  Hui.field_string' ~input_at ~at ~min_size ~label ~name:bot_honeypot_field ""

let buttons ?(force_rescue = false) g =
  let uf = Page.Gen.url_fmt g in
  let submit =
    let label = Uimsg.submit_suggestion in
    let r = Html_kit.htmlact_request uf (Suggestion.Url.v Create) in
    let t = Htmlact.target "form:up" in
    let e = Htmlact.effect' `Element in
    let q = Htmlact.query "form:up" in
    let rescue =
      Htmlact.query_rescue (if force_rescue then `Force else `Bool true)
    in
    let at = At.[t; r; e; q; rescue; Hui.Class.submit] in
    Hui.button ~at (El.txt label)
  in
  Hui.group ~align:`Start ~dir:`H [submit]

let too_many_suggestions =
  El.splice [
    El.p [El.txt Uimsg.suggestions_too_many];
    El.p [El.txt Uimsg.please_try_again_in_a_few_days]]

let need_a_doi_or_suggestion =
  El.p ~at:[Hclass.message; Hclass.error]
    [El.txt Uimsg.need_a_doi_or_suggestion]

let suggest_form ?invalid_user_doi ?force_rescue ?(msg = El.void) g s =
  let doi = doi_input ?invalid_user_doi g s in
  let suggestion = input_suggestion s in
  let comment = input_comment s in
  let email = input_email s in
  let at = At.[class' "suggest"] in
  Html_kit.form_no_submit ~at
    [ doi; msg; suggestion; comment; email; bot_honeypot;
      buttons ?force_rescue g]

let pending_anchor = "pending"

let doi s = match Suggestion.doi s with
| None -> El.void
| Some d as doi ->
    (* Why does doi_link take an option ? try to remove that. *)
    Html_kit.doi_link doi (El.txt (" doi:" ^ Doi.to_string d))


let created g s =
  (* FIXME move the messages to Uimsg *)
  let self = Suggestion.Url.v (Page {id = Suggestion.id s; created = true}) in
  let title = Uimsg.suggestion in
  let uf = Page.Gen.url_fmt g in
  let h1 = El.h1 [ El.txt "Suggestion submitted"] in
  let doi = doi s in
  let thanks = El.p [El.txt "Thank you!"] in
  let theref = El.p [El.txt "The reference "; doi; El.txt ":" ] in
  let sugg =
    El.p ~at:[Hclass.message; Hclass.info] [El.txt (Suggestion.suggestion s)]
  in
  let added =
    let pending = Kurl.Fmt.rel_url uf ~src:self ~dst:(Suggestion.Url.v Index) in
    let id = Suggestion.Id.to_int (Suggestion.id s) in
    let pending = String.concat "#" [pending; string_of_int id]in
    El.p [El.txt "has been added to the list of ";
          Html_kit.link ~href:pending (El.txt "pending suggestions.")]
  in
  let content = El.section [h1; thanks; theref; sugg; added] in
  Page.with_content g ~self ~title ~content

let confirm_delete g s =
  let uf = Page.Gen.url_fmt g in
  let cancel_button =
    let cancel = Suggestion.Url.v (View_fields (Suggestion.id s)) in
    Html_kit.htmlact_cancel_button uf cancel
  in
  let delete_button =
    let confirm = Suggestion.Url.v (Delete (Suggestion.id s)) in
    let target = Html_kit.target_entity in
    Html_kit.htmlact_delete uf confirm ~target (El.txt Uimsg.confirm_delete)
  in
  let bs = Hui.group ~align:`Justify ~dir:`H [delete_button; cancel_button] in
  let really = El.p [El.txt Uimsg.really_delete_suggestion; doi s; El.txt "?"]in
  let sugg =
    El.p ~at:[Hclass.message; Hclass.info] [El.txt (Suggestion.suggestion s)]
  in
  let email, email_delete = match Suggestion.email s with
  | "" -> El.void, El.void
  | email ->
      let subject = Uimsg.about_your_suggestion in
      let bib = Page.Gen.bibliography g in
      let body = Bibliography.suggester_email_message bib in
      let mailto = Html_kit.mailto_link ~subject ~body ~email (El.txt email) in
      El.p [El.txt Uimsg.you_may_want_to_send_an_email; El.sp; mailto; El.sp;
            El.txt Uimsg.to_notify_the_suggestion_was_treated],
      El.splice [El.sp; El.txt Uimsg.the_email_address_will_be_deleted]
  in
  let no_undo_warn =
    let warn = [El.txt Uimsg.this_cannot_be_undone; email_delete] in
    El.p ~at:[Hclass.message; Hclass.warn] warn
  in
  let at = At.[Hclass.entity; Hclass.editing; Hclass.fade] in
  El.section ~at [really; sugg; email; no_undo_warn; bs; ]

let view_timestamp g s =
  let time = Ptime.of_span (Ptime.Span.of_int_s (Suggestion.timestamp s)) in
  let time = Option.value ~default:Ptime.epoch time in
  let y, m, d = Ptime.to_date time in
  El.small [El.txt (Fmt.str "%04d-%02d-%02d" y m d)]

let view_private_email g s =
  let email = Suggestion.email s in
  if Page.Gen.only_public g || email = "" then El.void else
  let subject = Uimsg.about_your_suggestion in
  let mailto = Html_kit.mailto_link ~subject ~email (El.txt email) in
  El.em [El.txt Uimsg.suggested_by; El.sp; mailto; El.br ()]

let view_private g s =
  if Page.Gen.only_public g then El.void else
  let email = view_private_email g s in
  let comment = Suggestion.comment s in
  let comment = if comment = "" then El.void else El.txt comment in
  if El.is_void email && El.is_void comment then El.void else
  let at = [Hclass.private'; Hclass.Font.small; Hclass.Font.w_light] in
  El.p ~at [view_private_email g s; comment]

let view_suggestion g s =
  let at = [Hclass.Font.small; Hclass.Margin.top_0125] in
  El.p ~at [El.txt (Suggestion.suggestion s)]

let edit_ui g s =
  if not (Page.Gen.editable g) then El.void else
  let uf = Page.Gen.url_fmt g in
  let id = Suggestion.id s in
  let integrate = Suggestion.Url.v (Page {id; created = false}) in
  let integrate = Html_kit.htmlact_integrate_button uf integrate in
  let del =
    Html_kit.htmlact_delete_button uf (Suggestion.Url.v (Confirm_delete id))
  in
  let at = [Hclass.entity_ui; Hclass.Margin.top_000] in
  let ui = Hui.group ~at ~align:`Justify ~dir:`H [integrate; del] in
  El.splice [ui; El.hr ()]

let view_fields ?(no_ui = false) g ~self s =
  let id = Int.to_string (Suggestion.Id.to_int ((Suggestion.id s))) in
  let timestamp = view_timestamp g s in
  let preamble = El.p [timestamp; doi s] in
  let suggestion = view_suggestion g s in
  let private' = view_private g s in
  let ui = if no_ui then El.void else edit_ui g s in
  El.div ~at:[At.id id; Hclass.entity; Hclass.vspace_025; Hclass.fade]
    [ Html_kit.anchor_a id; preamble; suggestion; private'; ui]

let integrate_html g ~self s ~form =
  let h1 = El.h1 [El.txt Uimsg.integrate_suggestion] in
  El.section [h1; view_fields g ~self ~no_ui: true s; El.hr (); form]

let integrate g s ~form =
  let id = Suggestion.id s in
  let self = Suggestion.Url.v (Page { id; created = false}) in
  let content = integrate_html g ~self s ~form in
  Page.with_content g ~self ~title:Uimsg.suggestions ~content

let index_html g ~self ss ~is_full =
  let h1 = El.h1 [Html_kit.uppercase_span Uimsg.suggestions ] in
  let preamble = match Page.Gen.only_public g with
  | true ->
      if is_full then too_many_suggestions else
      let msg = El.p [El.txt Uimsg.suggestion_form_descr] in
      El.splice [msg; El.hr (); suggest_form g Suggestion.new'; El.hr ()]
  | false ->
      if ss = [] then El.p [El.txt Uimsg.no_pending_suggestions] else El.void
  in
  let pendings = match ss with
  | [] -> El.void
  | ss ->
      let count = Html_kit.item_count (List.length ss) in
      let h2 =
        El.h2 ~at:At.[id pending_anchor]
          [ Html_kit.anchor_a pending_anchor;
            El.txt Uimsg.pending_suggestions; El.sp; count];
      in
      let suggestions =
        El.div ~at:[Hclass.vspace_075] (List.map (view_fields g ~self) ss)
      in
      El.splice [h2; suggestions]
  in
  El.section [h1; preamble; pendings]

let index g ss ~is_full =
  let self = Suggestion.Url.v Index in
  let content = index_html g ~self ss ~is_full in
  Page.with_content g ~self ~title:Uimsg.suggestions ~content
