(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

(* Hc requests *)

let hc_request uf u =
  let meth, url = Kurl.Fmt.req uf u in
  Hc.request ~meth url

let target_entity = "." ^ (snd (At.to_pair Hclass.entity)) ^ ":up"
let target_entity_up = target_entity ^ " :up"

let hc_button ?(at = []) ?x_align ?align ?dir ?tip uf url ?query ?target label =
  let r = hc_request uf url in
  let t = At.if_some (Option.map Hc.target target) in
  let e = Hc.effect `Element in
  let q = match query with None -> At.void | Some q -> Hc.query q in
  Hui.button ~at:(r :: t :: e :: q :: at) ?x_align ?align ?dir ?tip label

let hc_delete ?(at = []) ?x_align ?align ?dir ?tip uf url ~target label =
  let r = hc_request uf url in
  let t = Hc.target target and e = Hc.effect `Element in
  Hui.delete ~at:(r :: t :: e :: at) ?x_align ?align ?dir ?tip label

let hc_cancel ?(at = []) ?x_align ?align ?dir ?tip uf url ~target label =
  let r = hc_request uf url in
  let t = Hc.target target and e = Hc.effect `Element in
  Hui.cancel ~at:(r :: t :: e :: at) ?x_align ?align ?dir ?tip label

let hc_edit_button ?(target = target_entity) uf url =
  let label = [Icon.pencil_alt; El.span [El.txt_of ellipsify Uimsg.edit]] in
  hc_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let hc_replace_button ?(target = target_entity) uf url =
  let label = [Icon.save_as; El.span [El.txt_of ellipsify Uimsg.replace]] in
  hc_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let hc_duplicate_button ?(target = target_entity) uf url =
  let label = [Icon.duplicate; El.span [El.txt_of ellipsify Uimsg.duplicate]] in
  hc_button uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let hc_delete_button ?(target = target_entity) uf url =
  let label = [Icon.trash; El.span [El.txt_of ellipsify Uimsg.delete]] in
  hc_delete uf url ~target ~x_align:`Center ~dir:`H (El.splice label)

let hc_cancel_button ?(target = target_entity) uf url =
  hc_cancel uf url ~target (El.txt Uimsg.cancel)

let new_entity_button ~href ~label =
  let label = [Icon.plus_sm; El.span [El.txt_of ellipsify label]] in
  Hui.button_link ~href ~x_align:`Center ~dir:`H (El.splice label)

(* Hc responses *)

let hc_page_location_update ?(init = Http.Headers.empty) uf url ~title () =
  let url = Kurl.Fmt.url uf url in
  let title = Hc.encode_location_title title in
  Http.Headers.(init
                |> def Hc.location_replace url
                |> def Hc.location_title title)

let hc_redirect ?(init = Http.Headers.empty) uf url =
  let url = Kurl.Fmt.url uf url in
  Http.Headers.(empty |> def Hc.redirect url)

(* Links and anchors *)

let anchor_href aid = At.href (Printf.sprintf "#%s" aid)
let anchor_a aid =
  El.a ~at:At.[anchor_href aid; class' "anchor"; v "aria-hidden" "true"] []

let link ?(at = []) ~href:h content = El.a ~at:(At.href h :: at) [content]

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
