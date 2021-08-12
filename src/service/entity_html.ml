(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* Description *)

let edit_description
    (type t) (module E : Entity.DESCRIBABLE with type t = t) ?textarea_at ?at e
  =
  let autogrow = true and min_rows = 2 in
  let label = El.txt Uimsg.description and col = E.description' in
  Hui.field_text ?textarea_at ?at ~autogrow ~min_rows ~label ~col e

let view_description
    (type t) (module E : Entity.DESCRIBABLE with type t = t)
    ?(at = []) e
  =
  match E.description e with
  | "" -> El.void
  | d ->
      let viz = if E.public e then At.void else Hclass.private' in
      let col_class = Hui.Class.for_col E.description' in
      El.p ~at:(Hclass.value :: col_class :: viz :: at) [El.txt d]

(* Note *)

let edit_note
    (type t) (module E : Entity.ANNOTABLE with type t = t) ?textarea_at ?at e
  =
  let autogrow = true and min_rows = 2 in
  let label = El.txt Uimsg.note and col = E.note' in
  Hui.field_text ?textarea_at ?at ~autogrow ~min_rows ~label ~col e

let view_note
    (type t) (module E : Entity.ANNOTABLE with type t = t) ?(at = []) e
  =
  match E.note e with
  | "" -> El.void
  | n ->
      let viz = if E.public e then At.void else Hclass.private' in
      let col_class = Hui.Class.for_col E.note' in
      El.p ~at:(Hclass.value :: col_class :: viz :: at) [El.txt n]

(* Public *)

let edit_public
    (type t) (module E : Entity.PUBLICABLE with type t = t) ?(at = [])  e
  =
  let label = El.txt Uimsg.public in
  Hui.field_bool ~label ~col:E.public' e

let viz (type t) (module E : Entity.PUBLICABLE with type t = t) e =
  if E.public e then At.void else Hclass.private'

(* Private note *)

let edit_private_note
    (type t) (module E : Entity.PRIVATELY_ANNOTABLE with type t = t)
    ?textarea_at ?at e
  =
  let autogrow = true and min_rows = 1 in
  let label = El.span ~at:[Hclass.private'] [El.txt Uimsg.private_note] in
  let col = E.private_note' in
  Hui.field_text ?textarea_at ?at ~autogrow ~min_rows ~label ~col e

let view_private_note
    (type t) (module E : Entity.PRIVATELY_ANNOTABLE with type t = t)
    g ?(at = []) e
  =
  if not (Page.Gen.private_data g) then El.void else
  match E.private_note e with
  | "" -> El.void
  | n ->
      let col_class = Hui.Class.for_col E.private_note' in
      El.p ~at:(Hclass.value :: col_class :: Hclass.private' :: at) [El.txt n]

(* Relations *)

let removable
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    ?(suff = El.void) ~key ~remove_tip ~render e
  =
  let input_id =
    let sid = string_of_int (E.id e) in
    El.input ~at:At.[hidden; name key; value sid] ()
  in
  let remove =
    let cls = [Hui.Class.button; Hclass.remove; Hclass.entity] in
    let at = At.type' "button" :: At.title remove_tip :: cls in
    El.button ~at [El.txt "\u{02DF}"]
  in
  let entity =
    let viz = if E.public e then At.void else Hclass.private' in
    let at = [viz; Hui.Class.for_table E.table; Hclass.value] in
    El.span ~at [render e; remove]
  in
  El.span ~at:[Hclass.removable] [input_id; entity; suff]

let addable
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    ~key ~add_tip ~render uf ~action e
  =
  let entity =
    let viz = if E.public e then At.void else Hclass.private' in
    let at = [viz; Hui.Class.for_table E.table; Hclass.value] in
    El.span ~at [render e]
  in
  let r = Hfrag.hc_request uf (action e) in
  let t = Hc.target ":up :up :up" in
  let e = Hc.effect `Element in
  let tab_index = At.tabindex 0 in
  let title = At.v "title" add_tip in
  let role = At.v "role" "option" in
  let at = [Hclass.addable; r; t; e; tab_index; title; role] in
  El.li ~at [entity]

let addable_list
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    ~key ~add_tip ~render uf ~action es
  =
  let li = addable (module E) ~key ~add_tip ~render uf ~action in
  El.ol ~at:[At.v "role" "listbox"] (List.map li es)

let person_key = function (* FIXME use that in reference_service *)
| None ->
    Reference.Contributor.(Hquery.key_for_rel table person')
| Some role ->
    let suff = Person.role_to_string role in
    Reference.Contributor.(Hquery.key_for_rel table person' ~suff)

let addable_subject_list uf ~parents ss =
  let render s =
    let p = Option.bind (Subject.parent s) (Fun.flip Id.Map.find_opt parents) in
    match p with
    | None -> El.txt (Subject.name s ^ "\u{207A}")
    | Some p ->
        El.txt (Fmt.str "%s\u{207A} (%s)" (Subject.name s) (Subject.name p))
  in
  let action s = Subject.Url.v (Select_add (Subject.id s)) in
  let key = Reference.Subject.(Hquery.key_for_rel table subject') in
  let add_tip = Uimsg.add_subject in
  addable_list (module Subject) ~key ~add_tip ~render uf ~action ss

let addable_contributor_list role uf ps =
  let render p = El.txt (Person.names_lf p ^ "\u{207A}") in
  let action p = Person.Url.v (Select_add (role, (Person.id p))) in
  let key = Reference.Subject.(Hquery.key_for_rel table subject') in
  let add_tip = match role with
  | Some Person.Author -> Uimsg.add_author
  | Some Person.Editor -> Uimsg.add_editor
  | None -> Uimsg.add_person
  in
  addable_list (module Person) ~key ~add_tip ~render uf ~action ps

let add_subject uf =
  let dl = addable_subject_list uf ~parents:Id.Map.empty [] in
  let input =
    let r = Hfrag.hc_request uf (Subject.Url.v (Select "")) in
    let t = Hc.target (":up :up ol") in
    let e = Hc.event ~debounce_ms:250 "input" in
    let eff = Hc.effect `Element in
    let pl = Uimsg.subject ^ "\u{207A}" in
    let min_size = String.length pl - 1 in
    let pl = At.placeholder pl in
    let ac = At.autocomplete "off" in
    let type' = At.type' "search" in
    let at = [Hclass.value; Hclass.subject; r; t; e; eff; ac; pl; type'] in
    Hui.input_string' ~at ~autogrow:true ~min_size ~name:"select" ""
  in
  let selector = El.div ~at:[Hclass.select_entity] [input; dl] in
  let at = [Hclass.add; Hclass.entity; Hclass.subject] in
  El.div ~at [selector]

let add_contributor role uf =
  let dl = addable_contributor_list role uf [] in
  let input =
    let r = Hfrag.hc_request uf (Person.Url.v (Select (role, ""))) in
    let t = Hc.target (":up :up ol") in
    let e = Hc.event ~debounce_ms:250 "input" in
    let eff = Hc.effect `Element in
    let pl = match role with
    | None -> Uimsg.person
    | Some Person.Author -> Uimsg.author
    | Some Person.Editor -> Uimsg.editor
    in
    let pl = pl ^ "\u{207A}" in
    let min_size = String.length pl - 1 in
    let pl = At.placeholder pl in
    let ac = At.autocomplete "off" in
    let type' = At.type' "search" in
    let at = [Hclass.value; Hclass.person; r; t; e; eff; ac; pl; type'] in
    Hui.input_string' ~at ~autogrow:true ~min_size ~name:"select" ""
  in
  let selector = El.div ~at:[Hclass.select_entity] [input; dl] in
  let at = [Hclass.add; Hclass.entity; Hclass.person] in
  El.div ~at [selector]

let comma = El.txt ",\u{00A0}"

let removable_subject =
  let key = Reference.Subject.(Hquery.key_for_rel table subject') in
  let render e = El.txt_of Subject.name e in
  let remove_tip = Uimsg.remove_subject in
  removable (module Subject) ~key ~remove_tip ~render ~suff:comma

let removable_contributor role =
  let key = person_key role in
  let render p = El.txt_of Person.names_lf p in
  let remove_tip = match role with
  | Some Author -> Uimsg.remove_author
  | Some Editor -> Uimsg.remove_editor
  | None -> Uimsg.remove_person
  in
  removable (module Person) ~key ~remove_tip ~render ~suff:comma


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
