(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std


let bad_val_400 ~kind k v =
  let trunc = String.sub v 0 (Int.min (String.length v) 10) in
  let dots = if String.length trunc <> String.length v then "…" else "" in
  let reason = Fmt.str "key %s: not a %s: %s%s" k kind trunc dots in
  Http.Resp.bad_request_400 ~reason ()

let no_key_400 ~kind k =
  let reason = Fmt.str "%s key %s not found" kind k in
  Http.Resp.bad_request_400 ~reason ()

(* Generic *)

type 'a kind = { kind : string; dec : string -> 'a option }
let kind kind dec = { kind; dec }
let bool = { kind = "bool"; dec = bool_of_string_opt }
let int = { kind = "int"; dec = int_of_string_opt }
let int64 = { kind = "int64"; dec = Int64.of_string_opt }
let float = { kind = "float"; dec = Float.of_string_opt }

let[@inline] kind_dec key kind s = match kind.dec s with
| None -> bad_val_400 ~kind:kind.kind key s | Some v -> Ok v

type 'a key = { name : string; kind : 'a kind }
let key name kind = { name; kind }
let[@inline] dec k s = kind_dec k.name k.kind s

let find k ~none q = match Http.Query.find k.name q with
| None -> Ok none | Some s -> dec k s

let find' k q = match Http.Query.find k.name q with
| None -> Ok None | Some s ->
    match k.kind.dec s with
    | None -> bad_val_400 ~kind:k.kind.kind k.name s
    | Some _ as r -> Ok r

let find_all k q =
  let rec loop acc = function
  | [] -> Ok (List.rev acc)
  | s :: ss ->
      match k.kind.dec s with
      | None -> bad_val_400 ~kind:k.kind.kind k.name s
      | Some v -> loop (v :: acc) ss
  in
  loop [] (Http.Query.find_all k.name q)


let get k q = match Http.Query.find k.name q with
| None -> no_key_400 ~kind:k.kind.kind k.name
| Some v -> dec k v

let get_all k q =
  if not (Http.Query.mem k.name q) then no_key_400 ~kind:k.kind.kind k.name else
  find_all k q

module Intset = Set.Make (Int)

let uniquify_ids l =
  let rec loop seen acc = function
  | [] -> List.rev acc
  | i :: is ->
      if Intset.mem i seen then loop seen acc is else
      loop (Intset.add i seen) (i :: acc) is
  in
  loop Intset.empty [] l

let find_ids ~uniquify key q = match Http.Query.find_all key q with
| [] -> Ok []
| ids ->
    let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | "" :: ids -> loop seen acc ids
    | i :: ids ->
        match int_of_string_opt i (* FIXME *)  with
        | None ->
            let reason = Fmt.str "key %s: %S is not an identifier" key i in
            Http.Resp.bad_request_400 ~reason ()
        | Some i when uniquify && Intset.mem i seen -> loop seen acc ids
        | Some i ->  loop (Intset.add i seen) (i :: acc) ids
    in
    loop Intset.empty [] ids


(* Generic rel *)

(* FIXME the following uses int_of_string et al. This is really
   NOT GOOD, e.g. for ids we should use custom types for IDs
   in the database fields. Also we need a generic encoding of options.  *)
let int_option_of_string = function
| "" -> Some None
| s -> Option.map Option.some (int_of_string_opt s)

let parse_kind' ~kind kind_of_string k v = match kind_of_string v with
| Some v -> Ok v | None -> bad_val_400 ~kind k v

let parse_kind ~kind kind_of_string col acc k v = match kind_of_string v with
| Some v -> Ok (Rel.Col.Value (col, v) :: acc)
| None -> bad_val_400 ~kind k v

let unhandled key t =
  let explain = Fmt.str "key %s: unhandled column type %a" key Rel.Type.pp t in
  Http.Resp.server_error_500 ~explain ()

let find_col :
  type a. ('r, a) Rel.Col.t -> none:a -> Http.query -> (a, Http.resp) result =
  fun col ~none q ->
  let key = Rel.Col.name col in
  match Http.Query.find key q with
  | None ->
      begin match (Rel.Col.type' col) with
      | Rel.Type.Bool -> (* HTML checkboxes work that way… *)
          Ok false
      | _ -> Ok none
      end
  | Some s ->
      match Rel.Col.type' col with
      | Rel.Type.Bool -> Ok true
      | Rel.Type.Int -> kind_dec key int s
      | Rel.Type.Int64 -> kind_dec key int64 s
      | Rel.Type.Float -> kind_dec key float s
      | Rel.Type.Text -> Ok s
      (* TODO *)
      | Rel.Type.Option _ as t -> unhandled key t
      | Rel.Type.Blob as t -> unhandled key t
      | t -> unhandled key t


let add_col_value (type c) ~col:(col : ('a, c) Rel.Col.t) q acc =
  let key = Rel.Col.name col in
  match Http.Query.find key q with
  | None ->
      begin match (Rel.Col.type' col) with
      | Rel.Type.Bool ->
          (* That's the way HTML checkboxes work :-( *)
          Ok (Rel.Col.Value (col, false) :: acc)
      | _ ->  Ok acc
      end
  | Some v ->
      match Rel.Col.type' col with
      | Rel.Type.Bool ->
          parse_kind ~kind:"bool" bool_of_string_opt col acc key v
      | Rel.Type.Int ->
          parse_kind ~kind:"int" int_of_string_opt col acc key v
      | Rel.Type.Int64 ->
          parse_kind ~kind:"int64" Int64.of_string_opt col acc key v
      | Rel.Type.Float ->
          parse_kind ~kind:"float" float_of_string_opt col acc key v
      | Rel.Type.Text -> Ok (Rel.Col.Value (col, v) :: acc)
      | Rel.Type.Option Rel.Type.Int ->
          parse_kind ~kind:"int option"
            int_option_of_string col acc key v
      | Rel.Type.Option _ as t -> unhandled key t
      | Rel.Type.Blob as t -> unhandled key t
      | t -> unhandled key t

let find_cols ~cols q =
  let rec loop q acc = function
  | [] -> Ok acc
  | Rel.Col.V col :: cs ->
      match add_col_value ~col q acc with
      | Ok acc -> loop q acc cs
      | Error _ as e -> e
  in
  loop q [] cols

let find_table_cols _t ~cols q = find_cols ~cols q
let careless_find_table_cols ?ignore t q =
  let cols = Rel.Table.cols ?ignore t in
  find_cols ~cols q

let key_for_rel ?suff t c =
  let l = match suff with None -> [] | Some suff -> [suff] in
  String.concat "." (Rel.Table.name t :: Rel.Col.name c :: l)

(* Hyperbib stuff *)


let is_undo = "is-undo"
let key_is_undo = key is_undo bool

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

let person_key = function
| None -> Reference.Contributor.(key_for_rel table person')
| Some role ->
    let suff = Person.role_to_string role in
    Reference.Contributor.(key_for_rel table person' ~suff)


(* FIXME this is retarded *)

let create_author_first = "x-author-first"
let create_author_last = "x-author-last"
let create_author_orcid = "x-author-orcid"

let create_editor_first = "x-editor-first"
let create_editor_last = "x-editor-last"
let create_editor_orcid = "x-editor-orcid"

let create_person_first = "x-person-first"
let create_person_last = "x-person-last"
let create_person_orcid = "x-person-orcid"

let create_person_keys = function
| Some Person.Author ->
    create_author_first, create_author_last, create_author_orcid
| Some Editor ->
    create_editor_first, create_editor_last, create_editor_orcid
| None ->
    create_person_first, create_person_last, create_person_orcid

let find_create_person ~public ~role q =
  let first, last, orcid = create_person_keys role in
  let ( let* ) = Option.bind in
  let* first_names = Http.Query.find first q in
  let* last_name = Http.Query.find last q in
  let* orcid = Http.Query.find orcid q in
  Option.some @@ Person.v
    ~id:0 ~last_name ~first_names ~orcid ~note:"" ~private_note:"" ~public ()


let find_create_persons ~public role q =
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
  let first, last, orcid = create_person_keys (Some role) in
  let firsts = Http.Query.find_all first q in
  let lasts = Http.Query.find_all last q in
  let orcids = Http.Query.find_all orcid q in
  loop [] firsts lasts orcids

let find_contributor_kind ~public role q =
  let id_key = person_key (Some role) in
  let creates = find_create_persons ~public role q in
  let rec loop acc creates = function
  | [] -> List.rev acc
  | "x" :: ids -> loop (List.hd creates :: acc) (List.tl creates) ids
  | "" :: ids -> loop acc creates ids
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
  with Failure e -> Http.Resp.bad_request_400 ~explain:e ()

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
