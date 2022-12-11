(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

(* Removable *)

let hui_remover = Jstr.v "data-hui-remover"
let hui_remover_prop = El.Prop.bool hui_remover
let remover e =
  if El.prop hui_remover_prop e then () else
  let on_pointerdown ev =
    Ev.prevent_default ev;
    Ev.stop_propagation ev;
  in
  let on_click ev =
    let rem = El.parent e |> Option.get |> El.parent |> Option.get in
    Ev.prevent_default ev;
    Ev.stop_propagation ev;
    Fut.await (Hc_page.Effect.feedback_remove ~target:rem Element) @@ fun () ->
    El.remove rem
  in
  ignore (Ev.listen Ev.click on_click (El.as_target e));
  ignore (Ev.listen Ev.pointerdown on_pointerdown (El.as_target e));
  El.set_prop hui_remover_prop true e

let remover_interactions () =
  let hui_remover = Jstr.v "hui-remove" in
  List.iter remover (El.find_by_class hui_remover)

(* Orderable *)

let hui_cleanup : unit Ev.type' = Ev.Type.create (Jstr.v "hui-cleanup")

let ordering_cl = Jstr.v "ordering"
let orderable_caret_cl = Jstr.v "orderable-caret"
let orderable_cl = Jstr.v "orderable"
let orderable_placeholder_cl = Jstr.v "orderable-placeholder"
let reordering_cl = Jstr.v "reordering"

let px x = Jstr.(of_float x + v "px")

let set_pos e x y =
  El.set_inline_style El.Style.left (px x) e;
  El.set_inline_style El.Style.top (px y) e

let set_size e w h =
  El.set_inline_style El.Style.width (px w) e;
  El.set_inline_style El.Style.height (px h) e

let orderable_peers e = match El.parent e with
| None -> []
| Some p ->
    List.filter (fun o -> not (o == e)) @@
    El.find_by_class ~root:p orderable_cl

let isect_vertical r o =
  (* [true] iff [r]'s top-left or bottom-left or is within [o]'s vertical
     bounds *)
  let rtop = El.bound_y r in
  let rbot = rtop +. El.bound_h r in
  ((El.bound_y o <= rtop && rtop <= El.bound_y o +. El.bound_h o) ||
   (El.bound_y o <= rbot && rbot <= El.bound_y o +. El.bound_h o))

let h_limit_left = 15.
let h_limit_right = 30.

let isect r o = (* [true] iff top-left or bottom-left of [r] is in [o]. *)
  let rleft = El.bound_x r in
  (El.bound_x o -. h_limit_left <= rleft
   && rleft <= El.bound_x o +. min (El.bound_w o) h_limit_right) &&
  isect_vertical r o

let isect_last r o =
  (* [true iff top-left or bottom-left within [o] height and not too far. *)
  let rleft = El.bound_x r in
  let oright = El.bound_x o +. El.bound_w o in
  (oright -. h_limit_left < rleft && rleft < oright +. h_limit_right) &&
  isect_vertical r o

let rec find_caret_location r = function
| [] -> None
| o :: os ->
    if isect r o then Some (`Before, o) else
    if os = [] && isect_last r o then Some (`After, o) else
    find_caret_location r os

let reordable reordering caret placeholder e =
  let off_x = ref 0. and off_y = ref 0. in
  let lpointerdown = ref None in
  let lpointerup = ref None in
  let lpointermove = ref None in
  let lhuicleanup = ref None in
  let unlisten r = match !r with
  | None -> () | Some l -> Ev.unlisten l
  in
  let on_pointermove ev = match !reordering with
  | None -> ()
  | Some (r, peers) ->
      let m = Ev.Pointer.as_mouse (Ev.as_type ev) in
      set_pos r (Ev.Mouse.page_x m -. !off_x) (Ev.Mouse.page_y m -. !off_y);
      El.remove caret;
      match find_caret_location r peers with
      | None -> ()
      | Some (ins, o) -> El.insert_siblings ins o [caret]
  in
  let rec on_pointerup ev =
    begin match !reordering with
    | None -> ()
    | Some (r, _) ->
        reordering := None;
        El.remove r;
        El.set_class reordering_cl false r;
        El.remove_inline_style El.Style.position r;
        El.remove_inline_style El.Style.left r;
        El.remove_inline_style El.Style.top r;
        El.remove_inline_style El.Style.z_index r;
        let after = match El.parent caret with
        | None -> placeholder
        | Some _ -> caret
        in
        El.insert_siblings `After after [r];
        El.remove caret;
        El.remove placeholder;
    end;
    unlisten lpointerup; unlisten lpointermove
  in
  let on_pointerdown ev = match !reordering with
  | Some _ -> ()
  | None ->
      Ev.prevent_default ev;
      let m = Ev.Pointer.as_mouse (Ev.as_type ev) in
      off_x := Ev.Mouse.offset_x m;
      off_y := Ev.Mouse.offset_y m;
      reordering := Some (e, orderable_peers e);
      El.set_class reordering_cl true e;
      set_size placeholder (El.bound_w e) (El.bound_h e);
      El.insert_siblings `Before e [placeholder];
      El.set_inline_style El.Style.position (Jstr.v "absolute") e;
      El.set_inline_style El.Style.z_index (Jstr.v "1000") e;
      set_pos e (Ev.Mouse.page_x m -. !off_x) (Ev.Mouse.page_y m -. !off_y);
      lpointerup :=
        Some (Ev.listen Ev.pointerup on_pointerup
                (Document.as_target G.document));
      lpointermove := Some
          (Ev.listen Ev.pointermove on_pointermove
             (Document.as_target G.document));
  in
  let rec on_cleanup ev =
    (* We need that to unregister the on_mousedown closure *)
    unlisten lpointerdown; unlisten lhuicleanup;
  in
  (* Remove a potential previous listener *)
  ignore (Ev.dispatch (Ev.create hui_cleanup) (El.as_target e));
  lpointerdown :=
    Some (Ev.listen Ev.pointerdown on_pointerdown (El.as_target e));
  lhuicleanup :=
    Some (Ev.listen hui_cleanup on_cleanup (El.as_target e))

let ordering_interactions () =
  let setup_ordering e =
    let reordering = ref None in
    let caret =
      let at = [At.class' orderable_caret_cl] in
      El.span ~at [El.span [El.txt' "\u{2198}"]]
    in
    let placeholder = El.span ~at:[At.class' orderable_placeholder_cl] [] in
    let os = El.find_by_class ~root:e orderable_cl in
    List.iter (reordable reordering caret placeholder) os
  in
  List.iter setup_ordering (El.find_by_class ordering_cl)

(* Entity finders *)

let finder_input = Jstr.v "data-hui-finder-input"
let finder_input_prop = El.Prop.bool finder_input

let finder_input_clear i =
  El.set_prop El.Prop.value Jstr.empty i;
  ignore (Ev.dispatch (Ev.create Ev.input) (El.as_target i))

let finder_input_first_result i =
  let (let*) = Option.bind in
  let* root = El.parent i in
  let* root = El.parent root in
  El.find_first_by_selector ~root (Jstr.v ".finder-result")

let finder_result_input r =
  let (let*) = Option.bind in
  let* root = El.parent r in
  let* root = El.parent root in
  El.find_first_by_selector ~root (Jstr.v ".finder-input")

let key_escape = Jstr.v "Escape"
let key_arrow_down = Jstr.v "ArrowDown"
let key_arrow_up = Jstr.v "ArrowUp"
let key_space = Jstr.v "Space"
let key_enter = Jstr.v "Enter"

let finder_input i =
  if El.prop finder_input_prop i then () else
  let on_keydown ev = match Ev.Keyboard.code (Ev.as_type ev) with
  | k when Jstr.equal k key_escape ->
      Ev.prevent_default ev; finder_input_clear i
  | k when Jstr.equal k key_arrow_down ->
      begin match finder_input_first_result i with
      | None -> ()
      | Some fst -> Ev.prevent_default ev; El.set_has_focus true fst;
      end
  | _ -> ()
  in
  ignore (Ev.listen Ev.keydown on_keydown (El.as_target i));
  El.set_prop finder_input_prop true i;
  ()

let finder_input_interactions () =
  let finder_input_cl = Jstr.v "finder-input" in
  List.iter finder_input (El.find_by_class finder_input_cl)

let finder_result = Jstr.v "data-hui-finder-result"
let finder_result_prop = El.Prop.bool finder_result
let finder_result r =
  if El.prop finder_result_prop r then () else
  let on_keydown ev = match Ev.Keyboard.code (Ev.as_type ev) with
  | k when Jstr.equal k key_escape ->
      begin match finder_result_input r with
      | None -> ()
      | Some i -> Ev.prevent_default ev; finder_input_clear i
      end
  | k when Jstr.equal k key_arrow_up ->
      begin match El.previous_sibling r with
      | Some el -> Ev.prevent_default ev; El.set_has_focus true el
      | None ->
          match finder_result_input r with
          | None -> ()
          | Some i -> Ev.prevent_default ev; El.set_has_focus true i
      end
  | k when Jstr.equal k key_arrow_down ->
      begin match El.next_sibling r with
      | Some el -> Ev.prevent_default ev; El.set_has_focus true el;
      | None -> ()
      end
  | k when Jstr.equal k key_space || Jstr.equal k key_enter ->
      Ev.prevent_default ev;
      El.click r
  | _ -> ()
  in
  ignore (Ev.listen Ev.keydown on_keydown (El.as_target r));
  El.set_prop finder_result_prop true r;
  ()

let finder_result_interactions () =
  let finder_result_cl = Jstr.v "finder-result" in
  List.iter finder_result (El.find_by_class finder_result_cl)

let finder_interactions () =
  finder_input_interactions ();
  finder_result_interactions ()

(* Installing interactions. *)

(* FIXME nail down a strategy for applying stuff on hc
   element insertions. *)

let install_interactions () =
  remover_interactions ();
  ordering_interactions ();
  finder_interactions ();
  ()

let on_hc_cycle_end _ev =
  (* The new data may have dois we need to replace. *)
  Doi_relinker.relink_dois ();
  install_interactions ();
  ()

let install_event_handlers () =
  let document = Document.as_target G.document in
  ignore (Ev.listen Hc_page.Ev.cycle_end on_hc_cycle_end document);
  ()

let main () =
  Hc_page.init ();
  Doi_relinker.install ();
  install_interactions ();
  install_event_handlers ();
  ()

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

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
