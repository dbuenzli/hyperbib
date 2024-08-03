(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* String functions *)

let sentencify s = (* A bit primitive. *)
  let is_punct = function '?' | '!' | '.' | ',' | ';' -> true | _ -> false in
  if s = "" then "" else
  if is_punct s.[String.length s - 1] then s else s ^ "."

let uncapitalize = String.uncapitalize_ascii

let title ~sub ~sup = String.concat " – " [sub; sup]

let ellipsify s = s ^ "…"

(* Htmlact requests *)

let htmlact_request uf u =
  let method', url = Kurl.Fmt.req uf u in
  Htmlact.request ~method' url

let target_entity = "." ^ (snd (At.to_pair Hclass.entity)) ^ ":up"
let target_entity_up = target_entity ^ " :up"

let htmlact_button
    ?(at = []) ?x_align ?align ?dir ?tip uf url ?query ?target label
  =
  let r = htmlact_request uf url in
  let t = At.if_some (Option.map Htmlact.target target) in
  let e = Htmlact.effect `Element in
  let q = match query with None -> At.void | Some q -> Htmlact.query q in
  Hui.button ~at:(r :: t :: e :: q :: at) ?x_align ?align ?dir ?tip label

let htmlact_delete ?(at = []) ?x_align ?align ?dir ?tip uf url ~target label =
  let r = htmlact_request uf url in
  let t = Htmlact.target target and e = Htmlact.effect `Element in
  Hui.delete ~at:(r :: t :: e :: at) ?x_align ?align ?dir ?tip label

let htmlact_cancel ?(at = []) ?x_align ?align ?dir ?tip uf url ~target label =
  let r = htmlact_request uf url in
  let t = Htmlact.target target and e = Htmlact.effect `Element in
  Hui.cancel ~at:(r :: t :: e :: at) ?x_align ?align ?dir ?tip label

let htmlact_edit_button ?(target = target_entity) uf url =
  let label = [Icon.pencil_alt; El.span [El.txt_of ellipsify Uimsg.edit]] in
  htmlact_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let htmlact_integrate_button ?(target = target_entity) uf url =
  let label = [Icon.arrow_down_on_square;
               El.span [El.txt_of ellipsify Uimsg.integrate]] in
  let href = Kurl.Fmt.url uf url in
  Hui.button_link ~href ~x_align:`Center ~dir:`H (El.splice label)

let htmlact_replace_button ?(target = target_entity) uf url =
  let label = [Icon.save_as; El.span [El.txt_of ellipsify Uimsg.replace]] in
  htmlact_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let htmlact_duplicate_button ?(target = target_entity) uf url =
  let label = [Icon.duplicate; El.span [El.txt_of ellipsify Uimsg.duplicate]] in
  htmlact_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let htmlact_delete_button ?(target = target_entity) uf url =
  let label = [Icon.trash; El.span [El.txt_of ellipsify Uimsg.delete]] in
  htmlact_delete uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let htmlact_cancel_button ?(target = target_entity) uf url =
  htmlact_cancel uf url ~target (El.txt Uimsg.cancel)

let new_entity_button ~href ~label =
  let label = [Icon.plus_sm; El.span [El.txt_of ellipsify label]] in
  Hui.button_link ~href ~x_align:`Center ~dir:`H (El.splice label)

(* Htmlact responses *)

let htmlact_page_location_update ?(init = Http.Headers.empty) uf url ~title () =
  let url = Kurl.Fmt.url uf url in
  let title = Htmlact.encode_location_title title in
  Http.Headers.(init
                |> def Htmlact.location_replace url
                |> def Htmlact.location_title title)

let htmlact_redirect ?(init = Http.Headers.empty) uf url =
  let url = Kurl.Fmt.url uf url in
  Http.Headers.(empty |> def Htmlact.redirect url)

let url_of_req_referer req = match Kurl.Bare.of_req_referer req with
| Ok ref -> Ok (Kurl.v Kurl.any ref)
| Error e -> Http.Response.bad_request_400 ~explain:e ()

(* Links and anchors *)

let anchor_href aid = At.href (Printf.sprintf "#%s" aid)
let anchor_a aid =
  El.a ~at:At.[anchor_href aid; class' "anchor"; v "aria-hidden" "true"] []

let link ?(at = []) ~href:h content = El.a ~at:(At.href h :: at) [content]

let doi_link ?(at = []) doi text = match doi with
| "" -> El.void
| doi ->
    let dlink = Fmt.str "%s/%s" Doi.default_resolver doi in
    let at = At.(href dlink :: v "data-doi" doi :: at) in
    El.a ~at [text]

let mailto_link ?(at = []) ?(body = "") ?(subject = "") ~email text =
  (* Do that properly at some point https://www.rfc-editor.org/rfc/rfc6068 *)
  let esc s =
    Http.Pct.encode `Uri_component @@
    String.concat "\r\n" (String.split_on_char '\n' s)
  in
  let subj = if subject = "" then "" else Fmt.str "subject=%s" (esc subject) in
  let body = if body = "" then "" else Fmt.str "body=%s" (esc body) in
  let query = String.concat "&" [subj; body] in
  let query = if query = "" then "" else "?" ^ query in
  let href = Fmt.str "mailto:%s%s" email query in
  El.a ~at:(At.href href :: at) [text]

(* Letter indexes *)

type letter = string
let letter_id l = l
let h2_letter l =
  let lid = letter_id l in
  El.h2 ~at:At.[id lid; Hclass.letter] [anchor_a lid; El.txt l]

let letters_nav letters =
  let a l = El.a ~at:[anchor_href (letter_id l); Hclass.letter] [El.txt l] in
  El.nav ~at:At.[Hclass.letter_index] (List.map a letters)

let form_no_submit ?(at = []) es =
  (* Somehow it seems very hard to avoid implicit submission [1] which
     is not really adapted to these forms. So we don't rely on the
     submit event to send the request.
     [1]: https://html.spec.whatwg.org/multipage/\
          form-control-infrastructure.html#implicit-submission *)
  let at = At.v "onsubmit" "return false;" :: at in
  El.form ~at es

(* Entity *)

let entity_form_no_submit ?(at = []) es =
  let at = Hclass.entity :: Hclass.editing :: at in
  form_no_submit ~at es

let entity_kind_index uf ~self ~kind url =
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:url in
  let subjects = link ~href (El.txt kind) in
  El.span ~at:[Hclass.entity_kind] [subjects; El.sp; El.txt "»"; ]


(* Entity links *)

type 'a linker = ?text:El.html -> Kurl.fmt -> self:Kurl.t -> 'a -> El.html

let link_year ?text uf ~self y =
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Year.Url.page y) in
  let text = match text with None -> El.txt_of (Fmt.str "%d") y | Some t -> t in
  link ~at:[Hclass.year; Hclass.value] ~href text

let link_person ?text uf ~self p =
  let viz = if Person.public p then At.void else Hclass.private' in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Person.Url.page p) in
  let text = match text with
  | Some t -> t
  | None -> El.txt_of Person.names_lf p
  in
  link ~at:[viz; Hclass.person; Hclass.value] ~href text

let link_container ?text uf ~self c =
  let viz = if Container.public c then At.void else Hclass.private' in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Container.Url.page c) in
  let text = match text with
  | Some t -> t
  | None -> El.txt (match Container.title c with "" -> Uimsg.unknown | t -> t)
  in
  link ~at:[viz; Hclass.container; Hclass.value] ~href text

let link_subject ?text uf ~self s =
  let viz = if Subject.public s then At.void else Hclass.private' in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Subject.Url.page s) in
  let text = match text with Some t -> t | None -> El.txt_of Subject.name s in
  link ~at:[viz; Hclass.subject; Hclass.value] ~href text

let link_label ?text uf ~self l =
  let viz = if Label.public l then At.void else Hclass.private' in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:(Label.Url.page l) in
  let text = match text with Some t -> t | None -> El.txt_of Label.name l in
  link ~at:[viz; Hui.Class.label; Hclass.value] ~href text

(* Miscellaneous *)

let item_count ?(at = []) count =
  let count = Printf.sprintf "(%d)" count in
  El.span ~at:(Hclass.item_count :: at) [El.txt count]

let uppercase_span ?(at = []) t =
  El.span ~at:(Hclass.uppercase :: at) [El.txt t]

let description ?(at = []) html = El.p ~at:(Hclass.description :: at) [html]
