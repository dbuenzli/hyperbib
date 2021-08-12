(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr_note
open Brr_note_kit
open Brr

let string_editor : Jstr.t signal -> El.t -> bool signal * Jstr.t event
  =
  fun str el ->
  let label = El.label [] in
  let editor =
    let at = At.[type' (Jstr.v "text"); class' (Jstr.v "edit")] in
    El.input ~at ()
  in
  let label_click = Evr.(on_el Ev.click (Fun.const true) label) in
  let input_change = Evr.(on_el Ev.change (Fun.const false) editor) in
  let input_blur = Evr.(on_el Ev.blur (Fun.const false) editor) in
  let keys = Evr.(on_el Ev.keydown Key.of_ev editor) in
  let escape_key = E.stamp (E.filter (Key.equal `Escape) keys) false in
  let return_key = E.stamp (E.filter (Key.equal `Return) keys) false in
  let start_edit = label_click in
  let stop_edit = E.select [input_change; escape_key; input_blur; return_key] in
  let editing = S.hold false (* FIXME *) (E.select [start_edit; stop_edit]) in
  let valid = S.hold true @@ E.select [start_edit; escape_key] in
  let action = S.sample_filter valid ~on:stop_edit @@ fun valid _ ->
    if valid then Some (El.prop El.Prop.value editor) else None
  in
  let () = Elr.def_children label (S.map (fun s -> [El.txt s]) str)
  and () = Elr.set_prop El.Prop.value ~on:(S.snapshot str ~on:start_edit) editor
  and () = Elr.(call (fun _ e -> El.select_text e) ~on:start_edit editor)
  and () = Elr.def_class (Jstr.v "editing") editing el
  and () = El.set_children el [label; editor] in
  editing, action

(* Quicly hacked and messy, needs reviewing with the right signal flow. *)

open Brr_note_legacy

let stored_doi_resolver : string Store.key = Store.key ()
let get_stored_doi_resolver () = Store.find stored_doi_resolver
let get_doi_resolver () = match get_stored_doi_resolver () with
| None | Some "" -> "https://doi.org" | Some resolver -> resolver

let doi_resolver, set_doi_resolver =
  let s, set = S.create (get_doi_resolver ()) in
  let set_doi_resolver s =
    let () = match s with
    | "" | "https://doi.org" -> Store.rem stored_doi_resolver
    | s ->
        match Uri.of_jstr (Jstr.v s) with
        | Error _ -> Store.rem stored_doi_resolver
        | Ok uri ->
            Store.add stored_doi_resolver (Jstr.to_string (Uri.to_jstr uri))
    in
    set (get_doi_resolver ())
  in
  s, set_doi_resolver

let at_data_doi = Jstr.v "data-doi"
let at_data_doi_sel = Jstr.v "[data-doi]"
let relink_dois () =
  let r = get_doi_resolver () in
  let r = match Uri.of_jstr (Jstr.v r) with
  | Error _ -> Jstr.v r
  | Ok uri -> (* Will add a final / to r if host only *) (Uri.to_jstr uri)
  in
  let relink r el () = match El.at at_data_doi el with
  | None -> ()
  | Some doi -> El.set_at At.Name.href (Some (Jstr.append r doi)) el
  in
  El.fold_find_by_selector (relink r) at_data_doi_sel ()

let doi_relinking doi_resolver span =
  let set_resolver r = set_doi_resolver (Jstr.to_string r) in
  let editing, edited = string_editor doi_resolver span in
  let () = Logr.may_hold (E.log edited set_resolver) in
  ()

let install () = match El.find_by_class (Jstr.v "doi-resolver") with
| [] -> ()
| spans ->
    let doi_resolver = S.map Jstr.v doi_resolver in
    List.iter (doi_relinking doi_resolver) spans;
    Logr.may_hold
      (* FIXME this should be part of the def of doi_resolver *)
      (E.log Store.ev (fun _ -> set_doi_resolver (get_doi_resolver ())));
    Logr.hold (S.log ~now:false doi_resolver (fun _ -> relink_dois ()));
    match get_stored_doi_resolver () with
    | None -> () | Some _ -> relink_dois ()

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
