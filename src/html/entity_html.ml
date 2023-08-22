(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* Description field *)

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

(* Note field *)

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

(* Private note field *)

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

(* Public field *)

let edit_public
    (type t) (module E : Entity.PUBLICABLE with type t = t) ?(at = [])  e
  =
  let label = El.txt Uimsg.public in
  Hui.field_bool ~label ~col:E.public' e

let viz (type t) (module E : Entity.PUBLICABLE with type t = t) e =
  if E.public e then At.void else Hclass.private'

(* Entity *)

let finder_result
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    ~creatable ~tip ~render uf ~action e
  =
  let entity =
    let viz = if E.public e || creatable then At.void else Hclass.private' in
    let at = [viz; Hui.Class.for_table E.table; Hclass.value] in
    El.span ~at [render e]
  in
  let r = Hfrag.htmlact_request uf (action e) in
  let t = Htmlact.target ":up :up :up" in
  let e = Htmlact.effect `Element in
  let tab_index = At.tabindex (-1) in
  let title = At.v "title" tip in
  let role = At.v "role" "option" in
  let c = if creatable then Hclass.creatable else At.void in
  let at = [Hui.Class.finder_result; c; r; t; e; tab_index; title; role] in
  El.li ~at [entity]

let input_finder_results
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    ~creatable ~tip ~render uf ~action es
  =
  let li = finder_result (module E) ~creatable:false ~tip ~render uf ~action in
  let c = match creatable with None -> El.void | Some c -> c in
  El.ol ~at:[At.v "role" "listbox"] (List.map li es @ [c])

let entity_input_finder ?min_size ?(at = []) uf ~kind req ~value =
  let r = Hfrag.htmlact_request uf req in
  let t = Htmlact.target (":up :up ol") in
  let e = Htmlact.event ~debounce_ms:250 "input" in
  let eff = Htmlact.effect `Element in
  let pl = kind ^ "\u{207A}" in
  let min_size = String.length pl (* - 1 *) in
  let pl = At.placeholder pl in
  let type' = At.type' "search" in
  let at = Hclass.value :: Hui.Class.finder_input :: at in
  let at = r :: t :: e :: eff :: pl :: type' :: at in
  Hui.input_string' ~at ~autogrow:true ~min_size ~name:"select" value

let entity_remove_button ~req ~tip =
  let at = match req with
  | None -> [Hclass.hui_remove]
  | Some (method', url) ->
      let r = Htmlact.request ~method' url in
      let t = Htmlact.target ":up :up" in
      let e = Htmlact.effect `Element in
      [r; t; e]
  in
  let at = Hui.Class.button :: Hclass.remove :: Hclass.entity :: at in
  let at = At.type' "button" :: At.title tip :: at in
  El.button ~at [El.txt "\u{02DF}"]

let comma = El.txt ",\u{00A0}"

let input_entity_id ~input_name:n eid =
  let eid = match eid with None -> "" | Some id -> string_of_int id in
  El.input ~at:At.[hidden; name n; value eid] ()

let entity_input_by_id
    (type t) (module E : Entity.PUBLICABLE with type t = t)
    uf ~input_name ~render ~orderable ~remove ~suff e
  =
  let input = input_entity_id ~input_name (Some (E.id e)) in
  let entity =
    let viz = if E.public e then At.void else Hclass.private' in
    let at = [viz; Hui.Class.for_table E.table; Hclass.value] in
    El.span ~at [render e; remove]
  in
  let at =
    At.if' orderable Hui.Class.orderable ::
    At.if' (not (El.is_void remove)) Hclass.removable :: []
  in
  El.span ~at [input; entity; suff]

(* Persons *)

let person_remove_button uf ~for_list ~input_name ~role =
  let tip = match role with
  | Some Person.Author -> Uimsg.remove_author
  | Some Person.Editor -> Uimsg.remove_editor
  | None -> Uimsg.remove_person
  in
  let req = match for_list with
  | true -> None
  | false ->
      let req = Person.Url.v (Input_finder (for_list, input_name, role)) in
      Some (Kurl.Fmt.req uf req)
  in
  entity_remove_button ~req ~tip

let person_input uf ~for_list ~input_name ~role p =
  let remove = person_remove_button uf ~for_list ~input_name ~role in
  let render p = El.txt_of Person.names_lf p in
  let orderable = for_list and suff = if for_list then comma else El.void in
  entity_input_by_id
    (module Person) uf ~input_name ~render ~orderable ~remove ~suff p

let person_input_create uf ~for_list ~input_name ~role p =
  let remove = person_remove_button uf ~for_list ~input_name ~role in
  let render p = El.txt_of Person.names_lf p in
  let input_create =
    let i k d = El.input ~at:At.[hidden; name k; value d] () in
    let first, last, orcid = Hquery.create_person_keys role in
    El.splice [
      i (Hquery.person_key role) "x";
      i first (Person.first_names p);
      i last (Person.last_name p);
      i orcid (Person.orcid p) ]
  in
  let entity =
    let create = Hclass.creatable in
    let at = [create; Hui.Class.for_table Person.table; Hclass.value] in
    El.span ~at [render p; remove]
  in
  let orderable = if for_list then Hui.Class.orderable else At.void in
  let suff = if for_list then comma else El.void in
  El.span ~at:[Hclass.removable; orderable] [input_create; entity; suff]

let person_creatable_result uf ~for_list ~input_name ~role p =
  let create = Uimsg.create_person in
  let tip = match role with
  | Some Person.Author -> Uimsg.create_and_add_author
  | Some Person.Editor -> Uimsg.create_and_add_editor
  | None -> Uimsg.create_and_add_person
  in
  let render p = El.txt (create ^ ": " ^ Person.names_lf p ^ "\u{207A}") in
  let action p = Person.Url.v (Input_create (for_list, input_name, role, p)) in
  finder_result (module Person) ~creatable:true ~tip ~render uf ~action p

let person_input_finder_results uf ~for_list ~input_name ~role ~creatable:p ps =
  let render p = El.txt (Person.names_lf p ^ "\u{207A}") in
  let action p =
    Person.Url.v (Input (for_list, input_name, role, Person.id p))
  in
  let tip = match role with
  | Some Person.Author -> Uimsg.add_author
  | Some Person.Editor -> Uimsg.add_editor
  | None -> Uimsg.add_person
  in
  let creatable =
    Option.map (person_creatable_result uf ~for_list ~input_name ~role) p
  in
  input_finder_results (module Person) ~creatable ~tip ~render uf ~action ps

let person_input_finder uf ~for_list ~input_name ~role =
  let res =
    person_input_finder_results uf ~for_list ~input_name ~role
      ~creatable:None []
  in
  let input_subject_id = input_entity_id ~input_name None in
  let input =
    let req =
      Person.Url.v (Input_finder_find (for_list, input_name, role, ""))
    in
    let kind = match role with
    | Some Person.Author -> Uimsg.author
    | Some Person.Editor -> Uimsg.editor
    | None -> Uimsg.person
    in
    entity_input_finder ~at:[Hclass.person] ~kind uf req ~value:""
  in
  let finder = El.div ~at:[Hui.Class.finder] [input; res] in
  let at = [Hclass.entity; Hclass.subject] in
  El.div ~at [input_subject_id; finder]

(* Subjects *)

let subject_remove_button uf ~for_list ~input_name =
  let tip = Uimsg.remove_subject in
  let req = match for_list with
  | true -> None (* client side removal *)
  | false ->
      let req = Subject.Url.v (Input_finder (for_list, input_name)) in
      Some (Kurl.Fmt.req uf req)
  in
  entity_remove_button ~req ~tip

let subject_input uf ~for_list ~input_name s =
  let remove = subject_remove_button uf ~for_list ~input_name in
  let render s = El.txt_of Subject.name s in
  let orderable = false and suff = if for_list then comma else El.void in
  entity_input_by_id
    (module Subject) uf ~input_name ~render ~orderable ~remove ~suff s

let subject_input_finder_results uf ~for_list ~input_name ~parents ss =
  let render s =
    let p = Option.bind (Subject.parent s) (Fun.flip Id.Map.find_opt parents) in
    match p with
    | None -> El.txt (Subject.name s ^ "\u{207A}")
    | Some p ->
        El.txt (Fmt.str "%s\u{207A} (%s)" (Subject.name s) (Subject.name p))
  in
  let action p = Subject.Url.v (Input (for_list, input_name, Subject.id p)) in
  let tip = Uimsg.add_subject and creatable = None in
  input_finder_results (module Subject) ~creatable ~tip ~render uf ~action ss

let subject_input_finder uf ~for_list ~input_name =
  let res =
    let parents = Id.Map.empty in
    subject_input_finder_results uf ~for_list ~input_name ~parents []
  in
  let input_subject_id = input_entity_id ~input_name None in
  let input =
    let req = Subject.Url.v (Input_finder_find (for_list, input_name, "")) in
    let kind = Uimsg.subject and at = [Hclass.subject] in
    entity_input_finder ~at ~kind uf req ~value:""
  in
  let finder = El.div ~at:[Hui.Class.finder] [input; res] in
  let at = [Hclass.entity; Hclass.subject] in
  El.div ~at [input_subject_id; finder]

(* Containers *)

let container_remove_button uf ~input_name =
  let tip = Uimsg.remove_container in
  let req = Kurl.Fmt.req uf (Container.Url.v (Input_finder input_name)) in
  entity_remove_button ~req:(Some req) ~tip

let container_input uf ~input_name c =
  let remove = container_remove_button uf ~input_name in
  let render c = El.txt_of Container.title c in
  let orderable = false and suff = El.void in
  entity_input_by_id
    (module Container) uf ~input_name ~render ~orderable ~remove ~suff c

let container_input_create uf ~input_name c =
  let remove = container_remove_button uf ~input_name in
  let input_create =
    let i k d = El.input ~at:At.[hidden; name k; value d] () in
    El.splice [
      i Hquery.create_container_title (Container.title c);
      i Hquery.create_container_isbn (Container.isbn c);
      i Hquery.create_container_issn (Container.issn c);
    ]
  in
  let entity =
    let create = Hclass.creatable in
    let at = [create; Hui.Class.for_table Container.table; Hclass.value] in
    El.span ~at [El.txt_of Container.title c; remove]
  in
  El.span ~at:[Hclass.removable] [input_create; entity]

let container_creatable_result uf ~input_name c =
  let create = Uimsg.create_container in
  let tip = Uimsg.create_and_add_container in
  let render p = El.txt (create ^ ": " ^ Container.title c ^ "\u{207A}") in
  let action p = Container.Url.v (Input_create (input_name, c)) in
  finder_result (module Container) ~creatable:true ~tip ~render uf ~action c

let container_input_finder_results uf ~input_name ~creatable:c cs =
  let render c = El.txt (Container.title c ^ "\u{207A}") in
  let action p = Container.Url.v (Input (input_name, Container.id p)) in
  let tip = Uimsg.add_container in
  let creatable = Option.map (container_creatable_result uf ~input_name) c in
  input_finder_results (module Container) ~creatable ~tip ~render uf ~action cs

let container_input_finder uf ~input_name =
  let res = container_input_finder_results uf ~input_name ~creatable:None [] in
  let input_container_id = input_entity_id ~input_name None in
  let input =
    let req = Container.Url.v (Input_finder_find (input_name, "")) in
    let r = Hfrag.htmlact_request uf req in
    let t = Htmlact.target (":up :up ol") in
    let e = Htmlact.event ~debounce_ms:250 "input" in
    let eff = Htmlact.effect `Element in
    let pl = Uimsg.container ^ "\u{207A}" in
    let pl = At.placeholder pl in
    let type' = At.type' "search" in
    let at = Hclass.value :: Hui.Class.finder_input :: Hclass.container :: [] in
    let at = r :: t :: e :: eff :: pl :: type' :: at in
    Hui.input_text' ~at ~autogrow:true ~min_rows:1 ~name:"select" ""
  in
  let finder = El.div ~at:[Hui.Class.finder] [input; res] in
  let at = [Hclass.entity; Hclass.container] in
  El.div ~at [input_container_id; finder]

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
