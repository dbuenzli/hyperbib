(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_text

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
    loc : Tloc.t; }

let v ~type' ~cite_key ~fields () = { type'; cite_key; fields; loc = Tloc.nil }

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
    match Url.scheme doi with
    | None -> ret doi
    | Some _ ->
        match Url.path_and_query doi with
        | None -> ret doi
        | Some p -> ret p

let keywords e = Option.map list_value (String.Map.find_opt "keywords" e.fields)
let annote e = String.Map.find_opt "annote" e.fields

(* Codec *)

type error_kind = string
type error = error_kind * B0_text.Tloc.t

let pp_error ppf (err, l) =
  Fmt.pf ppf "@[<v>%a:@,%a: %s@]"
    Tloc.pp l (Fmt.st [`Fg `Red]) "Error" err

let curr_char d = (* TODO better escaping (this is for error reports) *)
  Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

let err_illegal_uchar d = Tdec.err_here d "illegal character: %s" (curr_char d)
let err_illegal_byte d b = Tdec.err_here d "illegal character U+%04X" b
let err_expected d exp = Tdec.err_here d "expected %s" exp
let err_eoi msg d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "end of input: %s" msg

let err_eoi_entry = err_eoi "unclosed BibTeX entry"
let err_eoi_field = err_eoi "unfinished BibTeX entry field"
let err_eoi_value = err_eoi "unfinished BibTeX field value"
let err_brace d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "incorrect brace {} nesting"

let dec_byte d = match Tdec.byte d with
| c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
    err_illegal_byte d c
| c -> c
[@@ ocaml.inline]

let rec skip_white d = match dec_byte d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_white d
| _ -> ()

let dec_token ~stop d =
  let rec loop d = match dec_byte d with
  | 0x28 | 0x29 | 0x3B | 0x22
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
  | 0xFFFF -> Tdec.tok_pop d
  | c when c = stop -> Tdec.tok_pop d
  | _ -> Tdec.tok_accept_uchar d; loop d
  in
  loop d

let rec dec_string ~sbyte ~sline ~stop d = match dec_byte d with
| 0xFFFF -> err_eoi_value ~sbyte ~sline d
| c when c = stop -> Tdec.accept_byte d; Tdec.tok_pop d
| _ -> Tdec.tok_accept_uchar d; dec_string ~sbyte ~sline ~stop d

let rec dec_tex i ~sbyte ~sline d = match dec_byte d with
| 0xFFFF -> err_eoi_value ~sbyte ~sline d
| 0x007D ->
    if i = 0 then (Tdec.accept_byte d; Tdec.tok_pop d) else
    (Tdec.tok_accept_uchar d; dec_tex (i - 1) ~sbyte ~sline d)
| c ->
    let i = if c = 0x007B then i + 1 else i in
    Tdec.tok_accept_uchar d; dec_tex i ~sbyte ~sline d

let dec_value d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  match dec_byte d with
  | 0x007B (* { *) -> Tdec.accept_byte d; dec_tex 0 ~sbyte ~sline d
  | 0x0022 -> Tdec.accept_byte d; dec_string ~sbyte ~sline ~stop:0x0022 d
  | _ -> dec_token ~stop:0x002C d

let dec_field d acc =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  let id = dec_token ~stop:0x003D (* = *) d in
  skip_white d;
  match dec_byte d with
  | 0xFFFF -> err_eoi_field ~sbyte ~sline d
  | 0x003D (* = *) ->
      Tdec.accept_byte d;
      skip_white d;
      begin match dec_byte d with
      | 0xFFFF -> err_eoi_field ~sbyte ~sline d
      | _ ->
          String.Map.add (String.Ascii.lowercase id) (dec_value d) acc
      end
  | _ -> err_expected d "'='"

let rec dec_fields ~sbyte ~sline d acc =
  skip_white d;
  match dec_byte d with
  | 0xFFFF -> err_eoi_entry ~sbyte ~sline d
  | 0x007D (* } *) -> acc
  | _ ->
      let acc = dec_field d acc in
      skip_white d;
      match dec_byte d with
      | 0x002C (* , *) -> Tdec.accept_byte d; dec_fields ~sbyte ~sline d acc
      | 0x007D (* } *) -> acc
      | 0xFFFF -> err_eoi_entry ~sbyte ~sline d
      | b -> err_expected d "',' or '}'"

let dec_entry d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  Tdec.accept_byte d (* @ *);
  let type' = dec_token ~stop:0x007B d (* { *) in
  match dec_byte d with
  | 0x007B ->
      Tdec.accept_byte d;
      let cite_key = dec_token ~stop:0x002C d (* , *) in
      skip_white d;
      begin match dec_byte d with
      | 0x002C (* , *) ->
          Tdec.accept_byte d;
          let fields = dec_fields ~sbyte ~sline d String.Map.empty in
          let loc = Tdec.loc_to_here d ~sbyte ~sline in
          Tdec.accept_byte d;
          { type'; cite_key; fields; loc }
      | _ -> err_expected d "','"
      end
  | _ -> err_expected d "'{'"

let dec_entries d =
  let rec loop d acc =
    skip_white d;
    match dec_byte d with
    | 0x0040 (* @ *) -> loop d (dec_entry d :: acc)
    | 0xFFFF -> List.rev acc
    | b -> err_illegal_uchar d
  in
  loop d []

let of_string ?(file = Fpath.dash) s =
  try
    let file = Fpath.to_string file in
    let d = Tdec.create ~file s in
    Ok (dec_entries d)
  with Tdec.Err (loc, msg) -> Error (msg, loc)

let of_string' ?file s =
  Result.map_error (fun e -> Fmt.str "%a" pp_error e) @@
  (of_string ?file s)

let to_string es = Fmt.str "@[<v>%a@]" (Fmt.list pp) es
