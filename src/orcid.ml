(*---------------------------------------------------------------------------
   Copyright (c) 2025 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

type t = string

(* Presentations *)

let http_url = "http://orcid.org/"
let https_url = "https://orcid.org/"

let as_http_url id = http_url ^ id
let as_https_url id = https_url ^ id
let as_filename = Fun.id

(* Converting *)

let unsafe_of_string = Fun.id
let of_string s =
  let parse s =
    let len = String.length s in
    if len <> 19
    then Fmt.error "ORCID number length should be 19 found: %d" len else
    let csum = ref 0 in
    try
      for i = 0 to String.length s - 2 do
        if i = 4 || i = 9 || i = 14 then
          (if s.[i] = '-' then () else
           Fmt.failwith "ORCID expected character '-' at %d found: %C" i s.[i])
        else
        if Char.Ascii.is_digit s.[i]
        then csum := (!csum + Char.Ascii.digit_to_int s.[i]) * 2
        else Fmt.failwith "ORCID expected ASCII digit at %d found: %C" i s.[i]
      done;
      let rem = !csum mod 11 in
      let res = (12 - rem) mod 11 in
      let exp_last = if res = 10 then 'X' else Char.Ascii.digit_of_int res in
      let max = String.length s - 1 in
      if s.[max] = exp_last
      then Ok s else
      Fmt.failwith "ORCID checksum error, expected: %C found: %C"
        exp_last s.[max]
    with Failure e -> Error e
  in
  if String.starts_with ~prefix:http_url s
  then parse (String.subrange ~first:(String.length http_url) s) else
  if String.starts_with ~prefix:https_url s
  then parse (String.subrange ~first:(String.length https_url) s) else
  parse s

let v s = of_string s |> Result.get_ok'

let to_string = Fun.id
let pp = Fmt.string
let jsont =
  let dec = Jsont.Base.dec_result of_string in
  let enc = as_https_url in
  Jsont.Base.string (Jsont.Base.map ~kind:"ORCID" ~dec ~enc ())

(* Predicates and comparisons *)

let equal = String.equal
let compare = String.compare

(* Sets and maps *)

module T = struct type nonrec t = t let compare = compare end
module Set = Set.Make (T)
module Map = Map.Make (T)
