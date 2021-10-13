(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

let hui_def = Jstr.v "data-hui-def"
let hui_def_prop = El.Prop.bool hui_def

let entity_remover e =
  let on_click ev =
    let rem = El.parent e |> Option.get |> El.parent |> Option.get in
    Ev.prevent_default ev;
    Fut.await (Hc_page.Effect.feedback_remove ~target:rem Element) @@ fun () ->
    El.remove rem
  in
  Ev.listen Ev.click on_click (El.as_target e);
  El.set_prop hui_def_prop true e

let entity_remover_interactions () =
  let setup e = if El.prop hui_def_prop e then () else entity_remover e in
  let entity_remover = Jstr.v "hui-remove" in
  List.iter setup (El.find_by_class entity_remover)

let install_interactions () =
  entity_remover_interactions ();
  ()

let on_hc_cycle_end _ev =
  (* The new data may have dois we need to replace. *)
  (* FIXME nail down a strategy for applying stuff on hc
     element insertions. *)
  Doi_relinker.relink_dois ();
  install_interactions ();
  ()

let install_event_handlers () =
  let document = Document.as_target G.document in
  Ev.listen Hc_page.Ev.cycle_end on_hc_cycle_end document;
(*
  let unload ev =
    Ev.prevent_default ev;
    Jv.set (Ev.to_jv ev) "returnValue" (Jv.of_jstr @@ Jstr.v "Changes!");
  in
  Ev.listen (Ev.Type.void (Jstr.v "beforeunload")) unload
    (Window.as_target G.window);
*)
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
