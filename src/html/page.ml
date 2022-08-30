(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

module Gen = struct
  type auth_ui = [ `Login | `Logout ]
  type user_view = [ `Public | `Private ]
  type t =
    { now : Ptime.t;
      bibliography : Bibliography.t;
      url_fmt : Kurl.fmt;
      auth_ui : auth_ui option;
      user_view : user_view option;
      private_data : bool;
      testing : bool }

  let v ~now bibliography url_fmt ~auth_ui ~user_view ~private_data ~testing =
    { now; bibliography; url_fmt; auth_ui; user_view; private_data; testing }

  let now g = g.now
  let bibliography g = g.bibliography
  let url_fmt g = g.url_fmt
  let auth_ui g = g.auth_ui
  let user_view g = g.user_view
  let editable g = match g.user_view with
  | Some `Private -> true | Some `Public | None -> false

  let only_public g = not (g.private_data)
  let private_data g = g.private_data
  let testing g = g.testing
end

type t = { url : Kurl.t; doc : El.html; part : bool }
let v ~url ~doc ~part = { url; doc; part }
let url p = p.url
let doc p = p.doc
let part p = p.part
let doc_to_string p = El.to_string ~doc_type:(not p.part) p.doc

(* Page structure *)

let date stamp =
  let (y, m, d) = Ptime.to_date stamp in
  Fmt.str "%d-%02d-%02d" y m d

let frame g ~self ~title ~body =
  let uf = Gen.url_fmt g in
  let c = Gen.bibliography g in
  let url ~dst = Kurl.Fmt.rel_url uf ~src:self ~dst in
  let head =
    let generator = Fmt.str "hyperbib %s" Stamp.version in
    let viewport = At.content "width=device-width, initial-scale=1.0" in
    let script = At.src (url ~dst:(Static_file.Url.v Hyperbib_js)) in
    let css = At.href (url ~dst:(Static_file.Url.v Hyperbib_css)) in
    let favicon = match Bibliography.favicon_href c with
    | None -> "data:," (* avoid favicon requests *) | Some href -> href
    in
    El.head [
      El.meta ~at:At.[charset "utf-8"] ();
      El.meta ~at:At.[name "generator"; content generator] ();
      El.meta ~at:At.[name "viewport";  viewport] ();
      El.script ~at:At.[rel "text/javascript"; v "defer" "defer"; script] [];
      El.link
        ~at:At.[rel "stylesheet"; type' "text/css"; media "screen, print";css]
        ();
      El.link ~at:At.[rel "icon"; href favicon] ();
      El.title [El.txt title]]
  in
  let body =
    let cl, testing = match Gen.testing g with
    | false -> At.void, El.void
    | true ->
        Hclass.testing,
        El.header ~at:[Hclass.testing] [El.txt Uimsg.this_is_a_testing_site]
    in
    El.article ~at:[Hclass.hyperbib; cl] [testing; body]
  in
  El.html [ head; El.body [body] ]

let link_project g =
  let b = Gen.bibliography g in
  let href = Bibliography.project_href b in
  Hfrag.link ~href (El.txt_of Bibliography.project_title b)

let footer g =
  let c = Gen.bibliography g in
  let now = Gen.now g in
  El.footer
    [ El.txt (date now); El.sp; El.txt (Bibliography.copyrights c);
      El.txt " – "; link_project g; El.txt " – "; El.txt Stamp.version]

let body g ~ui ~content = El.splice [ui; content; footer g]

(* Basic ui *)

let link_bibtex_file g uf ~self =
  let bibtex_file = Bibliography.Url.bibtex_file (Gen.bibliography g) in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:bibtex_file in
  Hfrag.link ~href (El.splice [El.code [El.txt ".bib"]; El.txt " file"])

let menu_items =
  [ Uimsg.references, Reference.Url.v Index;
    Uimsg.containers, Container.Url.v Index;
    Uimsg.persons, Person.Url.v Index;
    Uimsg.subjects, Subject.Url.v Index;
    Uimsg.years, Year.Url.v Index ]

let menu ~self uf menu_item =
  let li (title, dst) =
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst in
    El.li [El.a ~at:[At.href href] [El.txt title]]
  in
  El.nav ~at:At.[class' "toc"] [El.ul (List.map li menu_items)]

let user_logout uf ~self =
  let goto = Kurl.Fmt.url uf self (* FIXME can't we use rel ? *) in
  let dst = User.Url.v (Logout { goto = Some goto }) in
  let act = Kurl.Fmt.rel_url uf ~src:self ~dst in
  El.form ~at:At.[method' "POST"; action act; Hclass.user_ui] [
    El.input ~at:At.[name User.Url.goto_key; value goto; type' "hidden"] ();
    Hui.submit (El.span ~at:[Hclass.logout] [El.txt (Uimsg.logout)]) ]

let user_login uf ~self =
  let goto = Some (Kurl.Fmt.url uf self) (* FIXME can we use rel ? *) in
  let dst = User.Url.v (Login { goto }) in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst  in
  El.span ~at:At.[Hclass.user_ui] [Hfrag.link ~href (El.txt Uimsg.login)]

let auth_ui g uf ~self = match Gen.auth_ui g with
| None -> El.void
| Some `Login -> user_login uf ~self
| Some `Logout -> user_logout uf ~self

let user_view g uf ~self view =
  let pub =
    let at = match view with
    | `Public -> At.[disabled]
    | `Private ->
        let req = Hfrag.hc_request uf (User.Url.v (View {private' = false})) in
        [req; Hc.effect `None]
    in
    Hui.button ~at ~tip:Uimsg.public_tip (El.txt Uimsg.public)
  in
  let priv =
    let at = match view with
    | `Private -> At.[disabled]
    | `Public ->
        let req = Hfrag.hc_request uf (User.Url.v (View {private' = true})) in
        [req; Hc.effect `None]
    in
    Hui.button ~at ~tip:Uimsg.private_tip (El.txt Uimsg.private')
  in
  let switch = Hui.group ~at:At.[Hclass.user_view] ~dir:`H[pub; El.sp; priv] in
  Hui.group ~x_align:`Center ~dir:`H [Icon.eye; El.sp; switch]

let user_view_ui ?(ui_ext = fun _ ~self:_ -> El.void) g uf ~self =
  match Gen.user_view g with
  | None -> El.void
  | Some view ->
      let labels = match view with
      | `Public -> El.void
      | `Private ->
          let label = Label.Url.v Index in
          let href = Kurl.Fmt.rel_url uf ~src:self ~dst:label in
          let at = At.[Hclass.edit_pages] in
          let link = El.span [Hfrag.link ~href (El.txt Uimsg.labels)] in
          Hui.group ~x_align:`Center ~dir:`H ~at [Icon.tag; link]
      in
      let user_view = user_view g uf ~self view in
      let ui_ext = ui_ext g ~self in
      let ui = [user_view; labels; ui_ext] in
      Hui.group ~at:At.[Hclass.edit_ui] ~dir:`V ui

let ui ?ui_ext g ~self =
  let uf = Gen.url_fmt g in
  let link_home =
    let home = Bibliography.Url.v Home in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst:home in
    Hfrag.link ~href (El.txt "BIB")
  in
  let link_help =
    let help = Bibliography.Url.v Help in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst:help in
    Hfrag.link ~href (El.txt Uimsg.help)
  in
  let auth_ui = auth_ui g uf ~self in
  let user_view_ui = user_view_ui ?ui_ext g uf ~self in
  let page_header =
    let c = Gen.bibliography g in
    let short_title = Bibliography.project_short_title c in
    let href = (Bibliography.project_href c) in
    El.header [
      El.h1 [Hfrag.link ~href (El.txt short_title)];
      El.div [El.em [link_home]; El.txt "  "; link_help;
              El.txt "  "; auth_ui; ]]
  in
  let menu = menu ~self uf menu_items in
  let downloads =
    El.div [El.span ~at:At.[class' "bibtex-file"] [link_bibtex_file g ~self uf]]
  in
  El.div ~at:At.[class' "ui"]
    (* Why this inner div is needed is unclear *)
    [ El.div [page_header; menu; downloads; Doi_html.resolver; user_view_ui]]

let full_title g ~title =
  let b = Gen.bibliography g in
  let proj = Bibliography.project_title b in
  if title = proj then proj else Hfrag.title ~sub:title ~sup:proj

let html ?ui_ext g ~self ~title ~content =
  let title = full_title g ~title in
  let body = body g ~ui:(ui ?ui_ext g ~self) ~content in
  { url = self; doc = frame g ~self ~title ~body; part = false }

let html_part g ~self ~content = { url = self; doc = content; part = true }
let html_404 ?ui_ext g ~kind ~self ~consult =
  let uf = Gen.url_fmt g in
  let kind_uncap = Hfrag.uncapitalize kind in
  let title = Uimsg.kind_not_found kind in
  let consult_href = Kurl.Fmt.rel_url uf ~src:self ~dst:consult in
  let consult_text = Uimsg.goto_kind_index kind_uncap in
  let content =
    El.section [
      El.h1 [ El.txt title ];
      El.p [ El.txt (Uimsg.kind_page_does_not_exist kind_uncap)];
      El.p [Hfrag.link ~href:consult_href (El.txt consult_text)]]
  in
  html ?ui_ext g ~self ~title ~content

let error_url_kind = Kurl.Kind.bare ~name:"error" ()
let error_url b = Kurl.v error_url_kind b
let error g req resp =
  let uf = Gen.url_fmt g in
  let bare = match Http.Headers.mem Hc.hc (Http.Req.headers req) with
  | false -> Kurl.Bare.of_req req
  | true ->
      (* Make links relative to the requesting page, not to the request *)
      match Kurl.Bare.of_req_referer req with
      | Ok b -> b
      | Error e -> Log.err (fun m -> m "%s" e); Kurl.Bare.of_req req
  in
  let self = error_url bare in
  let with_code title status = Fmt.str "%s (%d)" title status in
  let title, descr = match Http.Resp.status resp with
  | 401 -> Uimsg.unauthorized_401, Uimsg.unauthorized_401_descr
  | 404 -> Uimsg.not_found_404, Uimsg.not_found_404_descr
  | 501 -> Uimsg.not_implemented_501, Uimsg.not_implemented_501_descr
  | s when s >= 500 && s < 600 ->
      with_code Uimsg.server_error_5XX s, Uimsg.server_error_5XX_descr
  | s ->
      with_code Uimsg.something_went_wrong_XXX s,
      Uimsg.something_went_wrong_XXX_descr
  in
  let bib_url = Kurl.Fmt.rel_url uf ~src:self ~dst:(Bibliography.Url.v Home) in
  let goto_bib = Hfrag.link ~href:bib_url (El.txt Uimsg.go_back_to_bib) in
  let content =
    El.section [
      El.h1 [ El.txt title ];
      El.p [ El.txt descr; El.sp; goto_bib; ]]
  in
  let is_hc_req = Http.Headers.(mem (name "hc")) (Http.Req.headers req) in
  if is_hc_req
  then html_part g ~self ~content
  else html g ~self ~title ~content

(* Responses *)

let resp_part ?explain ?reason ?headers ?(status = Http.ok_200) part =
  Http.Resp.html ?explain ?reason ?headers status
    (El.to_string ~doc_type:false part)

let resp ?explain ?reason ?headers ?(status = Http.ok_200) p =
  Http.Resp.html ?explain ?reason ?headers status (doc_to_string p)

let resp_404 ?explain ?reason ?headers p =
  resp ?explain ?reason ?headers ~status:Http.not_found_404 p

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
