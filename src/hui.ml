(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hui programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

(* Classes *)

(* FIXME remove dependency on Hclass *)

module Class = struct

  let for_col c = At.class' (Ask.Col.name c)
  let for_table t = At.class' (Ask.Table.name t)

  let autogrow = At.class' "autogrow"
  let button = At.class' "button"
  let bool = At.class' "bool"
  let cancel = At.class' "cancel"
  let delete = At.class' "delete"
  let orderable = At.class' "orderable"
  let ordering = At.class' "ordering"
  let finder = At.class' "finder"
  let finder_input = At.class' "finder-input"
  let finder_result = At.class' "finder-result"
  let input = At.class' "input"
  let label = At.class' "label"
  let string = At.class' "string"
  let select = At.class' "select"
  let spinner = At.class' "spinner"
  let submit = At.class' "submit"
  let text = At.class' "text"

  let group = At.class' "group"
  let dir_h = At.class' "dir-h"
  let dir_v = At.class' "dir-v"
  let align_center = At.class' "align-center"
  let align_distribute = At.class' "align-distribute"
  let align_end = At.class' "align-end"
  let align_justify = At.class' "align-justify"
  let align_start = At.class' "align-start"
  let align_stretch = At.class' "align-stretch"
  let x_align_center = At.class' "x-align-center"
  let x_align_distribute = At.class' "x-align-distribute"
  let x_align_end = At.class' "x-align-end"
  let x_align_justify = At.class' "x-align-justify"
  let x_align_start = At.class' "x-align-start"
  let x_align_stretch = At.class' "x-align-stretch"
end

(* Group *)

type dir = [ `H | `V ]
type align = [ `Start | `End | `Center | `Justify | `Distribute | `Stretch ]

let align ~x = function
| `Start -> if x then Class.x_align_start else Class.align_start
| `End -> if x then Class.x_align_end else Class.align_end
| `Center -> if x then Class.x_align_center else Class.align_center
| `Justify -> if x then Class.x_align_justify else Class.align_justify
| `Distribute -> if x then Class.x_align_distribute else Class.align_distribute
| `Stretch -> if x then Class.x_align_stretch else Class.align_stretch

let dir = function `H -> Class.dir_h | `V -> Class.dir_v

let group ?(at = []) ?x_align:(xa = `Start) ?align:(a = `Start) ~dir:d html =
  let a = align ~x:false a and xa = align ~x:true xa in
  El.div ~at:(Class.group :: dir d :: a :: xa :: at) html

(* Buttons *)

let layout ?x_align:xa ?align:a ?dir:d () = match d, a, xa with
| Some _, _, _ | _, Some _, _ | _, _, Some _ ->
    let d = Option.value d ~default:`H in
    let a = align ~x:false @@ Option.value a ~default:`Start in
    let xa = align ~x:true @@ Option.value xa ~default:`Start in
    [Class.group; dir d; a; xa ]
| _ -> []

let _button ~type':t ?(at = []) ?x_align ?align ?dir ?tip label =
  let lats = layout ?x_align ?align ?dir () in
  let spinner = El.span ~at:At.[Class.spinner; v "aria-hidden" "true"] [] in
  let at = At.(Class.button :: type' t :: if_some "title" tip :: (lats @ at)) in
  El.button ~at [ spinner; label; ]

let button ?at ?x_align ?align ?dir ?tip ?(type' = "button") label =
  _button ~type' ?at ?x_align ?align ?dir ?tip label

let button_link ?(at = []) ?x_align ?align ?dir ?tip ~href:h label =
  let at = At.v "onclick" (Printf.sprintf "location.href='%s';" h) :: at in
  _button ~type':"button" ~at ?x_align ?align ?dir ?tip label

let submit ?(at = []) ?x_align ?align ?dir ?tip label =
  _button ~type':"submit" ~at:(Class.submit :: at) ?x_align ?align ?dir ?tip
    label

let cancel ?(at = []) ?x_align ?align ?dir ?tip label =
  _button ~type':"button" ~at:(Class.cancel :: at) ?x_align ?align ?dir ?tip
    label

let delete ?(at = []) ?x_align ?align ?dir ?tip label =
  _button ~type':"button" ~at:(Class.delete :: at) ?x_align ?align ?dir ?tip
    label

(* Editors *)

let input_string'
    ?(at = []) ?(autocomplete = true) ?(autogrow = false) ~min_size ~name v
  =
  let size = At.int "size" min_size in
  let name = At.name name in
  let value = At.value v in
  let ac = if autocomplete then At.autocomplete "off" else At.void in
  let at = Class.input :: Class.string :: ac :: size :: name :: value :: at in
  match autogrow with
  | false -> El.input ~at ()
  | true ->
      (* Needs corresponding CSS hack. See https://css-tricks.com/\
         the-cleanest-trick-for-autogrowing-textareas/ *)
      let set_value = "this.parentNode.dataset.value = this.value;" in
      let grow = At.v "oninput" set_value in
      let span_at = [Class.autogrow; Class.string; At.v "data-value" v] in
      El.span ~at:span_at [El.input ~at:(grow :: at) ()]

let input_text' ?(at = []) ?(autogrow = false) ~min_rows ~name v =
  let rows = At.rows min_rows in
  let name = At.name name in
  let at = Class.input :: Class.text :: rows :: name :: at in
  match autogrow with
  | false -> El.textarea ~at [El.txt v]
  | true ->
      (* Needs corresponding CSS hack. See https://css-tricks.com/\
         the-cleanest-trick-for-autogrowing-textareas/ *)
      let set_value = "this.parentNode.dataset.value = this.value;" in
      let grow = At.v "oninput" set_value in
      let div_at = [Class.autogrow; Class.text; At.v "data-value" v] in
      El.div ~at:div_at [El.textarea ~at:(grow :: at) [El.txt v]]

let input_bool' ?(at = []) ~name v =
  let name = At.name name in
  let value = At.value "true" in
  let type' = At.type' "checkbox" in
  let checked = At.if' v At.checked in
  let at = name :: type' :: value :: checked :: at in
  El.input ~at:(Class.input :: Class.bool :: at) ()

let input_bool ?at ~col r =
  let name = Ask.Col.name col and v = Ask.Col.proj col r in
  input_bool' ?at ~name v

let input_string ?at ?autocomplete ?autogrow ~min_size ~col r =
  let name = Ask.Col.name col and v = Ask.Col.proj col r in
  input_string' ?at ?autocomplete ?autogrow ~min_size ~name v

let input_text ?at ?autogrow ~min_rows ~col r =
  let name = Ask.Col.name col and v = Ask.Col.proj col r in
  input_text' ?at ?autogrow ~min_rows ~name v

let input_select ?(at = []) ~option_text ~option_value ~options ~col r =
  let rv = option_value (Ask.Col.proj col r) in
  let name = At.name (Ask.Col.name col) in
  let option o =
    let text = option_text o and ov = option_value o in
    let at = At.[if' (String.equal rv ov) selected; value ov] in
    El.option ~at [El.txt text]
  in
  let at = Class.input :: Class.select :: name :: at in
  El.select ~at (List.map option options)

let field_bool ?input_at ?(at = []) ~label ~col r =
  let label = El.span ~at:[Class.label] [label] in
  let input = input_bool ?at:input_at ~col r in
  let col_class = Class.for_col col in
  El.label ~at:(Hclass.field :: col_class :: at) [input; El.sp; label]


let field_string'
    ?input_at ?(at = []) ?autocomplete ?autogrow ~min_size ~label ~name v
  =
  let label = El.span ~at:[Class.label] [label] in
  let input =
    input_string' ?at:input_at ?autocomplete ?autogrow ~min_size ~name v
  in
  El.label ~at:(Hclass.field :: at) [label; El.sp; input]

let field_string
    ?input_at ?(at = []) ?autocomplete ?autogrow ~min_size ~label ~col r
  =
  let label = El.span ~at:[Class.label] [label] in
  let input =
    input_string ?at:input_at ?autocomplete ?autogrow ~min_size ~col r
  in
  let col_class = Class.for_col col in
  El.label ~at:(Hclass.field :: col_class :: at) [label; El.sp; input]

let field_text ?textarea_at ?(at = []) ?autogrow ~min_rows ~label ~col r =
  let label = El.span ~at:[Class.label] [label] in
  let input = input_text ?at:textarea_at ?autogrow ~min_rows ~col r in
  let col_class = Class.for_col col in
  El.label ~at:(Hclass.field :: col_class :: at) [label; El.sp; input]

let field_select
    ?select_at ?(at = []) ~label ~option_text ~option_value ~options ~col r
  =
  let label = El.span ~at:[Class.label] [label] in
  let input =
    input_select ?at:select_at ~option_text ~option_value ~options ~col r
  in
  let col_class = Class.for_col col in
  El.label ~at:(Hclass.field :: col_class :: at) [label; El.sp; input]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hui programmers

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
