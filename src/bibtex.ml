(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open More
open B0_text (* FIXME we should get rid of this *)

let escape = (* The escape rules are a bit unclear. These are those of LaTeX *)
  let tilde_esc = "\\textasciitilde" in
  let tilde_len = String.length tilde_esc in
  let circ_esc = "\\textasciicircum" in
  let circ_len = String.length circ_esc in
  let bslash_esc = "\\textbackslash" in
  let bslash_len = String.length bslash_esc in
  let char_len = function
  | '&' | '%' | '$' | '#' | '_' | '{' | '}' -> 2
  | '~' -> tilde_len
  | '^' -> circ_len
  | '\\' -> bslash_len
  | _ -> 1
  in
  let set_char b i = function
  | '&' | '%' | '$' | '#' | '_' | '{' | '}' as c ->
      Bytes.set b i '\\'; Bytes.set b (i + 1) c; i + 2
  | '~' -> Bytes.blit_string tilde_esc 0 b i tilde_len; i + tilde_len
  | '^' -> Bytes.blit_string circ_esc 0 b i circ_len; i + circ_len
  | '\\' -> Bytes.blit_string bslash_esc 0 b i bslash_len; i + bslash_len
  | c -> Bytes.set b i c; i + 1
  in
  String.byte_escaper char_len set_char

(* TODO unescape on decode. *)

type t =
  { type' : string;
    cite_key : string;
    fields : string String.Map.t;
    loc : Textloc.t; }

let v ~type' ~cite_key ~fields () =
  { type'; cite_key; fields; loc = Textloc.none }

let type' e = e.type'
let cite_key e = e.cite_key
let fields e = e.fields
let loc e = e.loc
let pp ppf e =
  let pp_field ppf (k, v) = Fmt.pf ppf "@[<h>%s = {%s}@]" k (escape v) in
  Fmt.pf ppf "@[<v2>@%s{%s,@,%a}@]" e.type' e.cite_key
    (Fmt.iter_bindings ~sep:Fmt.comma String.Map.iter pp_field) e.fields

(* Field values *)

let list_value s =
  List.filter (fun s -> s <> "") @@
  List.map String.trim (String.split_on_char ',' s)

let doi e = match String.Map.find_opt "doi" e.fields with
| None -> None
| Some doi ->
    let ret doi = match String.trim doi with
    | "" -> None
    | doi -> Some doi
    in
    (* chop scheme and authority in case there is one *)
    match Net.Url.scheme doi with
    | None -> ret doi
    | Some _ ->
        match Net.Url.path doi with
        | None -> ret doi
        | Some p -> ret p

let keywords e = Option.map list_value (String.Map.find_opt "keywords" e.fields)
let annote e = String.Map.find_opt "annote" e.fields

(* Codec *)

exception Error of Textloc.t * string

let err loc msg = raise_notrace (Error (loc, msg))
let err_span d ~start fmt =
  Format.kasprintf (err (Textdec.textloc_span d ~start)) fmt

let err_here d fmt = Format.kasprintf (err (Textdec.textloc d)) fmt

type error_kind = string
type error = error_kind * Textloc.t

let pp_error ppf (err, l) =
  Fmt.pf ppf "@[<v>%a:@,%a: %s@]"
    Textloc.pp l (Fmt.st [`Fg `Red]) "Error" err

let err_illegal_uchar d u =
  err_here d "illegal character: %a" Textdec.pp_decode u

let err_illegal_byte d b = err_here d "illegal character U+%04X" b
let err_expected d exp = err_here d "expected %s" exp
let err_eoi msg d ~start = err_span d ~start "end of input: %s" msg

let err_eoi_entry = err_eoi "unclosed BibTeX entry"
let err_eoi_field = err_eoi "unfinished BibTeX entry field"
let err_eoi_value = err_eoi "unfinished BibTeX field value"
let err_brace d ~start = err_span d ~start "incorrect brace {} nesting"

let nextc d =
  Textdec.next d;
  if Textdec.is_error d then err_here d "UTF-8 decoding error"

let uchar = Uchar.unsafe_of_int

let rec skip_white d = match Textdec.current d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> nextc d; skip_white d
| _ -> ()

let decode_token ~stop d =
  let rec loop d = match Textdec.current d with
  | 0x28 | 0x29 | 0x3B | 0x22
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
  | 0x11_000F -> Textdec.lexeme_pop d
  | c when c = stop -> Textdec.lexeme_pop d
  | u -> Textdec.lexeme_add d (uchar u); nextc d; loop d
  in
  loop d

let rec decode_string ~start ~stop d = match Textdec.current d with
| 0x11_000F -> err_eoi_value ~start d
| c when c = stop -> nextc d; Textdec.lexeme_pop d
| u -> Textdec.lexeme_add d (uchar u); nextc d; decode_string ~start ~stop d

let rec decode_tex i ~start d = match Textdec.current d with
| 0x11_000F -> err_eoi_value ~start d
| 0x007D as u ->
    if i = 0 then (nextc d; Textdec.lexeme_pop d) else
    (Textdec.lexeme_add d (uchar u); nextc d; decode_tex (i - 1) ~start d)
| c as u ->
    let i = if c = 0x007B then i + 1 else i in
    Textdec.lexeme_add d (uchar u); nextc d; decode_tex i ~start d

let decode_value d =
let start = Textdec.pos d in
  match Textdec.current d with
  | 0x007B (* { *) -> nextc d; decode_tex 0 ~start d
  | 0x0022 -> nextc d; decode_string ~start ~stop:0x0022 d
  | _ -> decode_token ~stop:0x002C d

let decode_field d acc =
let start = Textdec.pos d in
  let id = decode_token ~stop:0x003D (* = *) d in
  skip_white d;
  match Textdec.current d with
  | 0x11_000F -> err_eoi_field ~start d
  | 0x003D (* = *) ->
      nextc d;
      skip_white d;
      begin match Textdec.current d with
      | 0x11_000F -> err_eoi_field ~start d
      | _ ->
          String.Map.add (String.Ascii.lowercase id) (decode_value d) acc
      end
  | _ -> err_expected d "'='"

let rec decode_fields ~start d acc =
  skip_white d;
  match Textdec.current d with
  | 0x11_000F -> err_eoi_entry ~start d
  | 0x007D (* } *) -> acc
  | _ ->
      let acc = decode_field d acc in
      skip_white d;
      match Textdec.current d with
      | 0x002C (* , *) -> nextc d; decode_fields ~start d acc
      | 0x007D (* } *) -> acc
      | 0x11_000F -> err_eoi_entry ~start d
      | b -> err_expected d "',' or '}'"

let decode_entry d =
let start = Textdec.pos d in
  nextc d (* @ *);
  let type' = decode_token ~stop:0x007B d (* { *) in
  match Textdec.current d with
  | 0x007B ->
      nextc d;
      let cite_key = decode_token ~stop:0x002C d (* , *) in
      skip_white d;
      begin match Textdec.current d with
      | 0x002C (* , *) ->
          nextc d;
          let fields = decode_fields ~start d String.Map.empty in
          let loc = Textdec.textloc_span d ~start in
          nextc d;
          { type'; cite_key; fields; loc }
      | _ -> err_expected d "','"
      end
  | _ -> err_expected d "'{'"

let decode_entries d =
  let rec loop d acc =
    skip_white d;
    match Textdec.current d with
    | 0x0040 (* @ *) -> loop d (decode_entry d :: acc)
    | 0x11_000F -> List.rev acc
    | u -> err_illegal_uchar d u
  in
  loop d []

let of_string ?(file = Fpath.dash) s =
  try
    let file = Fpath.to_string file in
    let d = Textdec.make ~file s in
    Ok (nextc d; decode_entries d)
  with Error (loc, msg) -> Result.Error (msg, loc)

let of_string' ?file s =
  Result.map_error (fun e -> Fmt.str "%a" pp_error e) @@
  (of_string ?file s)

let to_string es = Fmt.str "@[<v>%a@]" (Fmt.list pp) es
