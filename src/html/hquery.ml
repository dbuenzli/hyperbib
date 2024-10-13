(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let bad_val_400 ~kind k v =
  let trunc = String.sub v 0 (Int.min (String.length v) 10) in
  let dots = if String.length trunc <> String.length v then "…" else "" in
  let reason = Fmt.str "key %s: not a %s: %s%s" k kind trunc dots in
  Http.Response.bad_request_400 ~reason ()

let no_key_400 ~kind k =
  let reason = Fmt.str "%s key %s not found" kind k in
  Http.Response.bad_request_400 ~reason ()

(* Generic *)

type 'a kind = { kind : string; dec : string -> 'a option }
let kind kind dec = { kind; dec }
let bool = { kind = "bool"; dec = bool_of_string_opt }
let int = { kind = "int"; dec = int_of_string_opt }
let int64 = { kind = "int64"; dec = Int64.of_string_opt }
let float = { kind = "float"; dec = Float.of_string_opt }
let string = { kind = "string"; dec = Option.some }

let[@inline] kind_dec key kind s = match kind.dec s with
| None -> bad_val_400 ~kind:kind.kind key s | Some v -> Ok v

type 'a key = { name : string; kind : 'a kind }
let key name kind = { name; kind }
let[@inline] dec k s = kind_dec k.name k.kind s

let find_first k ~none q = match Http.Query.find_first k.name q with
| None -> Ok none | Some s -> dec k s

let find_first' k q = match Http.Query.find_first k.name q with
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

let get k q = match Http.Query.find_first k.name q with
| None -> no_key_400 ~kind:k.kind.kind k.name
| Some v -> dec k v

let get_all k q =
  if not (Http.Query.mem k.name q) then no_key_400 ~kind:k.kind.kind k.name else
  find_all k q



let uniquify_ids (type id) (module Id : Rel_kit.ID with type t = id) l =
  let rec loop seen acc = function
  | [] -> List.rev acc
  | i :: is ->
      if Id.Set.mem i seen then loop seen acc is else
      loop (Id.Set.add i seen) (i :: acc) is
  in
  loop Id.Set.empty [] l


let find_ids
    (type id) (module Id : Rel_kit.ID with type t = id) ~uniquify key q
  =
  match Http.Query.find_all key q with
  | [] -> Ok []
  | ids ->
      let rec loop seen acc = function
      | [] -> Ok (List.rev acc)
      | "" :: ids -> loop seen acc ids
      | i :: ids ->
          match Id.of_string i with
          | Error e ->
              let reason = Fmt.str "key %s: %s" key e in
              Http.Response.bad_request_400 ~reason ()
          | Ok i when uniquify && Id.Set.mem i seen -> loop seen acc ids
          | Ok i ->  loop (Id.Set.add i seen) (i :: acc) ids
      in
      loop Id.Set.empty [] ids


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
  let explain =
    Fmt.str "key %s: unhandled column type %a" key Rel.Type.Repr.pp t
  in
  Http.Response.server_error_500 ~explain ()

let decode_col_value :
  type a. ('r, a) Rel.Col.t -> string -> (a, Http.Response.t) result =
  fun col s -> match Rel.Type.Repr.of_t (Rel.Col.type' col) with
  | Bool -> Ok true
  | Int -> kind_dec (Rel.Col.name col) int s
  | Int64 -> kind_dec (Rel.Col.name col) int64 s
  | Float -> kind_dec (Rel.Col.name col) float s
  | Text -> Ok s
  (* TODO *)
  | Option _ as t -> unhandled (Rel.Col.name col) t
  | Blob as t -> unhandled (Rel.Col.name col) t
  | t -> unhandled (Rel.Col.name col) t

let get_col :
  type a. ('r, a) Rel.Col.t -> Http.Query.t -> (a, Http.Response.t) result =
  fun col q ->
  let key = Rel.Col.name col in
  match Http.Query.find_first key q with
  | Some s -> decode_col_value col s
  | None ->
      begin match Rel.Type.Repr.of_t (Rel.Col.type' col) with
      | Bool -> (* HTML checkboxes work that way… *) Ok false
      | t -> no_key_400 ~kind:(Fmt.str "%a" Rel.Type.Repr.pp t) key
      end

let find_col :
  type a.
  ('r, a) Rel.Col.t -> none:a -> Http.Query.t -> (a, Http.Response.t) result
  =
  fun col ~none q ->
  let key = Rel.Col.name col in
  match Http.Query.find_first key q with
  | Some s -> decode_col_value col s
  | None ->
      begin match Rel.Type.Repr.of_t (Rel.Col.type' col) with
      | Bool -> (* HTML checkboxes work that way… *)
          Ok false
      | _ -> Ok none
      end

let add_col_value (type c) ~col:(col : ('a, c) Rel.Col.t) q acc =
  let key = Rel.Col.name col in
  match Http.Query.find_first key q with
  | None ->
      begin match Rel.Type.Repr.of_t (Rel.Col.type' col) with
      | Bool ->
          (* That's the way HTML checkboxes work :-( *)
          Ok (Rel.Col.Value (col, false) :: acc)
      | _ ->  Ok acc
      end
  | Some v ->
      match Rel.Type.Repr.of_t (Rel.Col.type' col) with
      | Bool ->
          parse_kind ~kind:"bool" bool_of_string_opt col acc key v
      | Int ->
          parse_kind ~kind:"int" int_of_string_opt col acc key v
      | Int64 ->
          parse_kind ~kind:"int64" Int64.of_string_opt col acc key v
      | Float ->
          parse_kind ~kind:"float" float_of_string_opt col acc key v
      | Text -> Ok (Rel.Col.Value (col, v) :: acc)
      | Option Int ->
          parse_kind ~kind:"int option"
            int_option_of_string col acc key v
      | Option Text ->
          let v = if v = "" (* XXX allow to trim *) then None else Some v in
          Ok (Rel.Col.Value (col, v) :: acc)
      | Option _ as t -> unhandled key t
      | Blob as t -> unhandled key t
      | t -> unhandled key t

let find_cols ~cols q =
  let rec loop q acc = function
  | [] -> Ok acc
  | Rel.Col.Def col :: cs ->
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
let find_date q = match Http.Query.find_first date_key q with
| None | Some "" -> Ok None
| Some s ->
    match Date.partial_of_string s with
    | v -> Ok (Some v)
    | exception Failure e -> Error e

let cite_key = "x-cite"
let find_cites q = Http.Query.find_all cite_key q

let create_container_title = "x-container-title"
let create_container_issn = "x-container-issn"
let create_container_isbn = "x-container-isbn"

let find_create_container q =
  match Http.Query.find_first create_container_title q with
  | None -> None
  | Some title ->
      let get o = Option.value ~default:"" o in
      let issn = get @@ Http.Query.find_first create_container_issn q in
      let isbn = get @@ Http.Query.find_first create_container_isbn q in
      let public = Http.Query.mem "public" q (* XXX Brittle *) in
      let id = Container.Id.zero in
      Option.some @@
      Container.make ~id ~title ~isbn ~issn ~note:"" ~private_note:"" ~public ()

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
  let* first_names = Http.Query.find_first first q in
  let* last_name = Http.Query.find_first last q in
  let* orcid = Http.Query.find_first orcid q in
  Option.some @@
  Person.make ~id:Person.Id.zero
    ~last_name ~first_names ~orcid ~note:"" ~private_note:"" ~public ()


let find_create_persons ~public role q =
  let rec loop acc fs ls os = match fs, ls, os with
  | first_names :: fs, last_name :: ls, orcid :: os ->
      let p =
        Person.make ~id:Person.Id.zero ~last_name ~first_names ~orcid
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
      | Some i ->
          let id = Person.Id.of_int i |> Result.get_ok' (* FIXME *) in
          loop (`Id id :: acc) creates ids
  in
  loop [] creates (Http.Query.find_all id_key q)

let find_create_contributors q =
  try
    let public = Http.Query.mem "public" q (* XXX Brittle *) in
    Ok
    (find_contributor_kind ~public Person.Author q,
     find_contributor_kind ~public Person.Editor q)
  with Failure e -> Http.Response.bad_request_400 ~explain:e ()
