(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(* Keep these words for now.
    let guest = El.small [El.txt "leave empty to login as guest"] in
    El.p [
      El.small [
        El.txt "If you don't have an account you can propose edits as
          guest by providing a valid email address. Your email address
          is not shared with third-parties and only retained by the system
          until the edit has been reviewed." ]
    ]; *)


(* FIXME move that back to hfrag. *)

let input_string' ?(type' = "text") ?(autocomplete = "off") ?(at = []) ~name v =
  let size = At.int "size" (max 5 (String.length v + 3)) in
  let auto = At.autocomplete autocomplete in
  let name = At.name name in
  let value = At.value v in
  let type' = At.type' type' in
  let at = size :: auto :: name :: type' :: value :: at in
  El.input ~at:(Hui.Class.input :: Hui.Class.string :: at) ()

let field_string' ?type' ?autocomplete ?input_at ?(at = []) ~label ~name v =
  let label = El.span ~at:[Hui.Class.label] [label] in
  let input = input_string' ?type' ?autocomplete ?at:input_at ~name v in
  El.label ~at:(Hclass.field :: Hui.Class.string :: at) [label; El.sp; input]

let username_field =
  let email = El.txt Uimsg.email and or_username = El.txt Uimsg.or_username in
  let label = El.splice [email; El.sp; El.small [or_username]] in
  let name = User.Url.username_key in
  let input_at = At.[required] in
  field_string' ~autocomplete:"username" ~input_at ~name ~label ""

let password_field =
  let label = El.splice [El.txt Uimsg.password; ] in
  let type' = "password" in
  let name = User.Url.password_key in
  field_string' ~type' ~autocomplete:"current-password" ~name ~label ""

let goto_field goto =
  El.input ~at:At.[name User.Url.goto_key; value goto; type' "hidden"] ()

let login_form g ~goto =
  let uf = Page.Gen.url_fmt g in
  let form_req = Kurl.v User.Url.kind (Authenticate { goto }) in
  let act = Kurl.Fmt.url uf form_req in
  let goto = match goto with None -> El.void | Some goto -> goto_field goto in
  let submit = Hui.submit (El.txt (Uimsg.login)) in
  let at = At.[method' "POST"; action act; Hclass.login] in
  El.form ~at [ goto; username_field; password_field; submit; ]

let login_html uf ~self ~msg ~goto =
  let h1 = El.h1 [El.txt Uimsg.login] in
  let msg = El.p [El.txt msg] in
  El.section [ h1; msg; login_form uf ~goto ]

let login g ~msg ~goto =
  let self = Kurl.V (User.Url.kind, Login { goto }) in
  let content = login_html g ~self ~msg ~goto in
  Page.with_content g ~self ~title:Uimsg.login ~content

let page_html g ~self username =
  El.section [ El.p [El.txt "Logged in as "; El.txt username ]]

let page g ~username =
  let self = Kurl.V (User.Url.kind, Login { goto = None }) in
  let title = Html_kit.title ~sub:username ~sup:Uimsg.user in
  let content = page_html g ~self username in
  Page.with_content g ~self ~title ~content
