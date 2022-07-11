(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Hyperbib.Std
open Rel

(* A much much much better model for partial would be a range
   of two unix timestamps, but I couldn't find a way to mark it work
   well in the db. *)

type day = int
type month = int
type year = int

(* md partial *)

type md_partial = month * day option

let md_partial_dec s =
  let i = int_of_string in
  try match String.split_on_char '-' s with
  | [m] -> Ok (i m, None)
  | [m; d] -> Ok (i m, Some (i d))
  | _ -> failwith ""
  with
  | Failure _ -> Fmt.error "%S: not of the form MM[-DD]" s

let md_partial_enc (m, d) = match d with
| None -> Ok (Fmt.str "%02d" m)
| Some d ->  Ok (Fmt.str "%02d-%02d" m d)

let md_partial_of_string = md_partial_dec
let pp_md_partial ppf v = Fmt.string ppf (md_partial_enc v |> Result.get_ok)

let ask_md_partial_type =
  let name = "date-md-partial" and pp = pp_md_partial in
  let coded = Type.Coded.v ~name ~pp md_partial_enc md_partial_dec Type.Text in
  Type.Coded coded

(* partial *)

type partial = year * md_partial option

let enc_partial (y, md) = match md with
| None -> Ok (Fmt.str "%04d" y)
| Some (m, None) -> Ok (Fmt.str "%04d-%02d" y m)
| Some (m, Some d) -> Ok (Fmt.str "%04d-%02d-%02d" y m d)

let dec_partial s =
  let i = int_of_string in
  try match String.split_on_char '-' s with
  | [y] -> Ok (i y, None)
  | [y; m] -> Ok (i y, Some (i m, None))
  | [y; m; d] -> Ok (i y, Some (i m, Some (i d)))
  | _ -> failwith ""
  with
  | Failure _ -> Fmt.error "%S: not of the form YYYY[-MM[-DD]]" s

let partial_of_string = dec_partial
let partial_to_string v = enc_partial v |> Result.get_ok
let pp_partial ppf v = Fmt.string ppf (enc_partial v |> Result.get_ok)

let ask_partial_type =
  let name = "date-partial" and pp = pp_partial in
  let coded = Type.Coded.v ~name ~pp enc_partial dec_partial Type.Text in
  Type.Coded coded

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
