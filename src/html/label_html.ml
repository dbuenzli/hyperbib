(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Result.Syntax

let ui_ext g ~self = match Page.Gen.editable g with
| false -> El.void
| true ->
    let uf = Page.Gen.url_fmt g in
    let new_button =
      let cancel = Some (Kurl.Fmt.url uf self) in
      let dst = Label.Url.v (Edit_new { cancel }) in
      let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
      Hui.button_link ~href (El.txt (Uimsg.new_label ^ "…"))
    in
    Hui.group ~at:At.[Hclass.entity_ui] ~dir:`H [new_button]

(*
let htmlact_request rf r = Hfrag.htmlact_request rf (Kurl.v Label.Url.kind r)
let buttons_view rf l =
  let edit_button =
    let r = htmlact_request rf @@ Edit (Label.id l) in
    let t = Htmlact.target ".form:up" and e = Hc.effect `Element in
    Hui.button ~at:[r; t; e] (El.txt "Edit")
  in
  Hui.group ~dir:`H [edit_button]

let buttons_edit rf l =
  let cancel_button =
    let r = htmlact_request rf @@ Edit (Label.id l) in
    let t = Htmlact.target ".form:up" and e = Hc.effect `Element in
    Hui.cancel ~at:[r; t; e] (El.txt "Cancel")
  in
  Hui.group ~align:`Justify ~dir:`H
    [cancel_button; Hui.submit (El.txt "Save label")]

let buttons ~edit rf l =
  if edit
  then buttons_edit rf l
  else buttons_view rf l

*)
let form ~edit rf l = El.void
(*
  let at = match edit with
  | false -> []
  | true ->
      let r = htmlact_request rf @@ Update (Label.id l) in
      let e = Htmlact.effect `Element in
      [r; e]
  in
  Hui.form ~at ~edit [
    Hui_ask.text_field ~edit ~col:Label.name' ~label:"Name" l;
    Hui_ask.text_field ~edit ~col:Label.synopsis' ~label:"Synopsis" l;
    Hui_ask.text_field ~edit ~col:Label.note' ~label:"Note" l;
    Hui_ask.text_field ~edit ~col:Label.private_note' ~label:"Private note" l;
    buttons ~edit rf l;
  ]
*)

let label rf l = El.void
(*
  El.section [
    El.h1 [El.txt ("Label " ^ Label.name l)];
    form ~edit:false rf l;
    El.h2 [El.txt "Applied to"]]
*)

let index_html g ~self ls =
  let uf = Page.Gen.url_fmt g in
  let anchor_id s = Fmt.str "%d" (Label.id s) in
  let label l =
    let viz = if Label.public l then At.void else Hclass.private' in
    let label = Hfrag.link_label uf ~self l in
    let label_descr = match Label.note l with
    | "" -> El.void
    | d -> El.p ~at:At.[Hclass.field; Hclass.description; viz] [El.txt d]
    in
    let lid = anchor_id l in
    El.li ~at:At.[id lid] [Hfrag.anchor_a lid; label; El.sp; label_descr]
  in
  let title = [Hfrag.uppercase_span Uimsg.labels ] in
  let labels = List.map label ls in
  let labels = El.nav ~at:At.[Hclass.index; Hui.Class.label] labels in
  El.section [ El.h1 title; labels ]

let index g ls =
  let self = Kurl.v Label.Url.kind Index in
  let content = index_html g ~self ls in
  Page.with_content ~ui_ext g ~self ~title:Uimsg.labels ~content
