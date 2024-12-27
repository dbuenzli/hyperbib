(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

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

(* Pages *)

type t = { url : Kurl.t; html : El.html; }
let v ~url ~html = { url; html }
let url p = p.url
let html p = p.html

(* Basic ui *)

let doi_resolver =
  El.span ~at:At.[class' "doi-resolver-ui"]
    [ El.span [El.txt "Full text resolver:"]; El.sp;
      El.span ~at:At.[class' "doi-resolver"]
        (* FIXME commenting that is better than anything
           but we still glitch on page reloads on FF. Not sure
           why this is the case, update should happen before
           the DOM is shown. *)
        [(* El.label [El.txt Doi.default_resolver] *)]]

let link_bibtex_file g uf ~self =
  let bibtex_file = Bibliography.Url.bibtex_file (Gen.bibliography g) in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:bibtex_file in
  Html_kit.link ~href (El.splice [El.code [El.txt ".bib"]; El.txt " file"])

let link_suggest_page g uf ~self =
  let suggest = Suggestion.Url.v Index in
  let href = Kurl.Fmt.rel_url uf ~src:self ~dst:suggest in
  let txt = match Gen.user_view g with
  | None | Some `Public -> Uimsg.make_a_suggestion
  | Some `Private -> Uimsg.suggestions
  in
  Html_kit.link ~href (El.txt txt)

let menu_secondary g uf ~self =
  El.nav ~at:At.[Hclass.secondary]
    [El.ul
       [ El.li [link_bibtex_file g ~self uf];
         El.li [link_suggest_page g ~self uf];
         El.li [doi_resolver];]]

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
  El.nav ~at:At.[Hclass.toc] [El.ul (List.map li menu_items)]

let user_logout uf ~self =
  let goto = Kurl.Fmt.url uf self (* FIXME can't we use rel ? *) in
  let dst = User.Url.v (Logout { goto = Some goto }) in
  let act = Kurl.Fmt.rel_url uf ~src:self ~dst in
  El.form ~at:At.[method' "POST"; action act; Hclass.user_ui] [
    El.input ~at:At.[name User.Url.goto_key; value goto; type' "hidden"] ();
    Hui.submit (El.span ~at:[Hclass.logout] [El.txt (Uimsg.logout)]) ]

let user_login uf ~self =
  let login_bare = Kurl.Fmt.bare uf (User.Url.v (Login { goto = None })) in
  let self_bare = Kurl.Fmt.bare uf self in
  let href = match Kurl.Bare.path login_bare = Kurl.Bare.path self_bare with
  | true -> Kurl.Fmt.url uf self (* We are on the login page, goto self *)
  | false ->
      let goto = Some (Kurl.Fmt.url uf self) in
      let dst = User.Url.v (Login { goto }) in
      Kurl.Fmt.rel_url uf ~src:self ~dst
  in
  El.span ~at:At.[Hclass.user_ui] [Html_kit.link ~href (El.txt Uimsg.login)]

let auth_ui g uf ~self = match Gen.auth_ui g with
| None -> El.void
| Some `Login -> user_login uf ~self
| Some `Logout -> user_logout uf ~self

let user_view g uf ~self view =
  let pub =
    let at = match view with
    | `Public -> At.[disabled]
    | `Private ->
        let req =
          Html_kit.htmlact_request uf (User.Url.v (View {private' = false}))
        in
        [req; Htmlact.effect `None]
    in
    Hui.button ~at ~tip:Uimsg.public_tip (El.txt Uimsg.public)
  in
  let priv =
    let at = match view with
    | `Private -> At.[disabled]
    | `Public ->
        let req =
          Html_kit.htmlact_request uf (User.Url.v (View {private' = true}))
        in
        [req; Htmlact.effect `None]
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
          let link = El.span [Html_kit.link ~href (El.txt Uimsg.labels)] in
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
    Html_kit.link ~href (El.txt "BIB")
  in
  let link_help =
    let help = Bibliography.Url.v Help in
    let href = Kurl.Fmt.rel_url uf ~src:self ~dst:help in
    Html_kit.link ~href (El.txt Uimsg.help)
  in
  let auth_ui = auth_ui g uf ~self in
  let user_view_ui = user_view_ui ?ui_ext g uf ~self in
  let page_header =
    let c = Gen.bibliography g in
    let short_title = Bibliography.project_short_title c in
    let href = (Bibliography.project_href c) in
    El.header [
      El.h1 [Html_kit.link ~href (El.txt short_title)];
      El.div [El.em [link_home]; El.txt "  "; link_help;
              El.txt "  "; auth_ui; ]]
  in
  let menu = menu ~self uf menu_items in
  let menu_secondary = menu_secondary g uf ~self in
  El.div ~at:At.[class' "ui"]

    (* Why this inner div is needed is unclear *)
    [ El.div [page_header; menu; menu_secondary; user_view_ui]]

(* Page frame *)

let full_title g ~title =
  let b = Gen.bibliography g in
  let proj = Bibliography.project_title b in
  if title = proj then proj else Html_kit.title ~sub:title ~sup:proj

let head g ~self ~title =
  let url ~dst = Kurl.Fmt.rel_url (Gen.url_fmt g) ~src:self ~dst in
  let title = full_title g ~title in
  let generator = Fmt.str "hyperbib %s" Stamp.version in
  let viewport = "width=device-width, initial-scale=1.0, viewport-fit=cover" in
  let script = At.src (url ~dst:(Static_file.Url.v Hyperbib_js)) in
  let css = At.href (url ~dst:(Static_file.Url.v Hyperbib_css)) in
  let favicon = match Bibliography.favicon_href (Gen.bibliography g) with
  | None -> "data:," (* avoid favicon requests *) | Some href -> href
  in
  let style css =
    At.[rel "stylesheet"; type' "text/css"; media "screen, print"; css]
  in
  El.head [
    El.meta ~at:At.[charset "utf-8"] ();
    El.meta ~at:At.[name "generator"; content generator] ();
    El.meta ~at:At.[name "viewport"; content viewport] ();
    El.script ~at:At.[rel "text/javascript"; v "defer" "defer"; script] [];
    El.link ~at:(style css) ();
    El.link ~at:At.[rel "icon"; href favicon] ();
    El.title [El.txt title]]

let footer g =
  let b = Gen.bibliography g in
  let date =
    let (y, m, d) = Ptime.to_date (Gen.now g) in
    Fmt.str "%d-%02d-%02d" y m d
  in
  let link_project =
    let href = Bibliography.project_href b in
    Html_kit.link ~href (El.txt_of Bibliography.project_title b)
  in
  El.footer
    [ El.txt date; El.sp; El.txt (Bibliography.copyrights b);
      El.txt " – "; link_project; El.txt " – "; El.txt Stamp.version]

let testing_header =
  El.header ~at:[Hclass.testing] [El.txt Uimsg.this_is_a_testing_site]

let page ?ui_ext g ~self ~title ~content =
  let head = head g ~self ~title in
  let ui = ui ?ui_ext g ~self in
  let cl, testing =
    if Gen.testing g then Hclass.testing, testing_header else At.void, El.void
  in
  let body =
    El.article ~at:[Hclass.hyperbib; cl] [testing; ui; content; footer g]
  in
  El.html [head; El.body [body]]

let with_content ?ui_ext g ~self ~title ~content =
  { url = self; html = page g ~self ~title ?ui_ext ~content }

let for_404 ?ui_ext g ~kind ~self ~consult =
  let uf = Gen.url_fmt g in
  let kind_uncap = Html_kit.uncapitalize kind in
  let title = Uimsg.kind_not_found kind in
  let consult_href = Kurl.Fmt.rel_url uf ~src:self ~dst:consult in
  let consult_text = Uimsg.goto_kind_index kind_uncap in
  let content =
    El.section [
      El.h1 [El.txt title];
      El.p [El.txt (Uimsg.kind_page_does_not_exist kind_uncap)];
      El.p [Html_kit.link ~href:consult_href (El.txt consult_text)]]
  in
  with_content ?ui_ext g ~self ~title ~content

(* Responses *)

let part_response ?headers ?log ?reason ?(status = Http.Status.ok_200) part =
  let html = El.to_string ~doctype:false part in
  Http.Response.html ?headers ?log ?reason status html

let response ?headers ?log ?reason ?(status = Http.Status.ok_200) p =
  let html = El.to_string ~doctype:true p.html in
  Http.Response.html ?headers ?log ?reason status html

let response_404 ?headers ?log ?reason p =
  response ?headers ?log ?reason ~status:Http.Status.not_found_404 p

(* Errors *)

let error g request response' =
  let uf = Gen.url_fmt g in
  let is_htmlact_req =
    Http.Headers.mem Htmlact.htmlact (Http.Request.headers request)
  in
  let self =
    let bare =
      if not is_htmlact_req then Kurl.Bare.of_req request else
      (* Make links relative to the requesting page, not to the request *)
      match Kurl.Bare.of_req_referer request with
      | Ok b -> b
      | Error e -> Log.err (fun m -> m "%s" e); Kurl.Bare.of_req request
    in
    Kurl.v Kurl.any bare
  in
  let with_code title status = Fmt.str "%s (%d)" title status in
  let title, descr = match Http.Response.status response' with
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
  let goto_bib = Html_kit.link ~href:bib_url (El.txt Uimsg.go_back_to_bib) in
  let content =
    El.section [ El.h1 [El.txt title]; El.p [El.txt descr; El.sp; goto_bib;]]
  in
  let log = Http.Response.log response' in
  let headers = Http.Response.headers response' in
  let status = Http.Response.status response' in
  let reason = Http.Response.reason response' in
  if is_htmlact_req
  then part_response ~reason ~log ~headers ~status content else
  let page = with_content g ~self ~title ~content in
  response ~reason ~log ~headers ~status page
