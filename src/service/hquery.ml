(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

(* Request parsing *)

(* FIXME the following uses int_of_string et al. This is really
   NOT GOOD, e.g. for ids we should use custom types for IDs
   in the database fields. Also we need a generic encoding of options.  *)

let int_option_of_string = function
| "" -> Some None
| s -> Option.map Option.some (int_of_string_opt s)

let parse_kind ~kind kind_of_string col acc k v = match kind_of_string v with
| Some v -> Ok (Ask.Col.Value (col, v) :: acc)
| None ->
    let reason = Fmt.str "key %s: not a %s" k kind in
    Resp.bad_request_400 ~reason ()

let unhandled key t =
  let explain = Fmt.str "key %s: unhandled column type %a" key Type.pp t in
  Resp.server_error_500 ~explain ()

let add_col_value (type c) ~col:(col : ('a, c) Col.t) q acc =
  let key = Ask.Col.name col in
  match Http.Query.find key q with
  | None ->
      begin match Col.type' col with
      | Type.Bool ->
          (* That's the way HTML checkboxes work :-( *)
          Ok (Ask.Col.Value (col, false) :: acc)
      | _ ->  Ok acc
      end
  | Some v ->
      match Col.type' col with
      | Type.Bool ->
          parse_kind ~kind:"bool" bool_of_string_opt col acc key v
      | Type.Int ->
          parse_kind ~kind:"int" int_of_string_opt col acc key v
      | Type.Int64 ->
          parse_kind ~kind:"int64" Int64.of_string_opt col acc key v
      | Type.Float ->
          parse_kind ~kind:"float" float_of_string_opt col acc key v
      | Type.Text -> Ok (Ask.Col.Value (col, v) :: acc)
      | Type.Option Type.Int ->
          parse_kind ~kind:"int option"
            int_option_of_string col acc key v
      | Type.Option _ as t -> unhandled key t
      | Type.Blob as t -> unhandled key t
      | t -> unhandled key t

let find_cols ~cols q =
  let rec loop q acc = function
  | [] -> Ok acc
  | Col.V col :: cs ->
      match add_col_value ~col q acc with
      | Ok acc -> loop q acc cs
      | Error _ as e -> e
  in
  loop q [] cols

let find_table_cols _t ~cols q = find_cols ~cols q
let careless_find_table_cols ?ignore t q =
  let cols = Table.cols ?ignore t in
  find_cols ~cols q

let key_for_rel ?suff t c =
  let l = match suff with None -> [] | Some suff -> [suff] in
  String.concat "." (Table.name t :: Col.name c :: l)

module Intset = Set.Make (Int)

let find_ids ~uniquify key q = match Http.Query.find_all key q with
| [] -> Ok []
| ids ->
    let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | i :: ids ->
        match int_of_string_opt i (* FIXME *)  with
        | None ->
            let reason = Fmt.str "key %s: not an identifier" key in
            Resp.bad_request_400 ~reason ()
        | Some i when uniquify && Intset.mem i seen -> loop seen acc ids
        | Some i ->  loop (Intset.add i seen) (i :: acc) ids
    in
    loop Intset.empty [] ids

let date_key = "x-date"
let find_date q = match Http.Query.find date_key q with
| None | Some "" -> Ok None
| Some s -> Result.map Option.some (Date.partial_of_string s)

let cite_key = "x-cite"
let find_cites q = Http.Query.find_all cite_key q

let create_container_title = "x-container-title"
let create_container_issn = "x-container-issn"
let create_container_isbn = "x-container-isbn"

let find_create_container q =
  match Http.Query.find create_container_title q with
  | None -> None
  | Some title ->
      let get o = Option.value ~default:"" o in
      let issn = get @@ Http.Query.find create_container_issn q in
      let isbn = get @@ Http.Query.find create_container_isbn q in
      let public = Http.Query.mem "public" q (* XXX Brittle *) in
      Option.some @@
        Container.v
          ~id:0 ~title ~isbn ~issn ~note:"" ~private_note:""
          ~public ()

let person_key = function (* FIXME use that in reference_service *)
| None -> Reference.Contributor.(key_for_rel table person')
| Some role ->
    let suff = Person.role_to_string role in
    Reference.Contributor.(key_for_rel table person' ~suff)


let create_author_first = "x-author-first"
let create_author_last = "x-author-last"
let create_author_orcid = "x-author-orcid"

let create_editor_first = "x-editor-first"
let create_editor_last = "x-editor-last"
let create_editor_orcid = "x-editor-orcid"

let contributor_keys = function
| Some Person.Author ->
    create_author_first, create_author_last, create_author_orcid
| Some Editor ->
    create_editor_first, create_editor_last, create_editor_orcid
| None -> assert false


let find_create_contributor ~public role q =
  let rec loop acc fs ls os = match fs, ls, os with
  | first_names :: fs, last_name :: ls, orcid :: os ->
      let p =
        Person.v ~id:0 ~last_name ~first_names ~orcid
          ~note:"" ~private_note:"" ~public ()
      in
      loop (`To_create p :: acc) fs ls os
  | [], [], [] -> List.rev acc
  | _ -> Fmt.failwith "create %s list mismatch" (Person.role_to_string role)
  in
  let first, last, orcid = contributor_keys (Some role) in
  let firsts = Http.Query.find_all first q in
  let lasts = Http.Query.find_all last q in
  let orcids = Http.Query.find_all orcid q in
  loop [] firsts lasts orcids

let find_contributor_kind ~public role q =
  let id_key = person_key (Some role) in
  let creates = find_create_contributor ~public role q in
  let rec loop acc creates = function
  | [] -> List.rev acc
  | "x" :: ids -> loop (List.hd creates :: acc) (List.tl creates) ids
  | id :: ids ->
      match int_of_string_opt id (* FIXME *)  with
      | None -> Fmt.failwith "key %s: %s not an identifier" id_key id
      | Some i -> loop (`Id i :: acc) creates ids
  in
  loop [] creates (Http.Query.find_all id_key q)

let find_create_contributors q =
  try
    let public = Http.Query.mem "public" q (* XXX Brittle *) in
    Ok
    (find_contributor_kind ~public Person.Author q,
     find_contributor_kind ~public Person.Editor q)
  with Failure e -> Resp.bad_request_400 ~explain:e ()

let uniquify_ids l =
  let rec loop seen acc = function
  | [] -> List.rev acc
  | i :: is ->
      if Intset.mem i seen then loop seen acc is else
      loop (Intset.add i seen) (i :: acc) is
  in
  loop Intset.empty [] l

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
