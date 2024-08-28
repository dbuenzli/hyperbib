(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(* A much much much better model for partial would be a range of two
   unix timestamps, but I couldn't find a way to make it work well in
   the db. *)

type day = int
type month = int
type year = int

(* md partial *)

type md_partial = month * day option
let md_partial_of_string s =
  let i = int_of_string in
  try match String.split_on_char '-' s with
  | [m] -> (i m, None)
  | [m; d] -> (i m, Some (i d))
  | _ -> failwith ""
  with
  | Failure _ -> Fmt.failwith "%S: not of the form MM[-DD]" s

let md_partial_to_string (m, d) = match d with
| None -> Fmt.str "%02d" m
| Some d ->  Fmt.str "%02d-%02d" m d

let pp_md_partial ppf v = Fmt.string ppf (md_partial_to_string v)

(* partial *)

type partial = year * md_partial option

let partial_of_string s =
  let i = int_of_string in
  try match String.split_on_char '-' s with
  | [y] -> (i y, None)
  | [y; m] -> (i y, Some (i m, None))
  | [y; m; d] -> (i y, Some (i m, Some (i d)))
  | _ -> failwith ""
  with
  | Failure _ -> Fmt.failwith "%S: not of the form YYYY[-MM[-DD]]" s

let partial_to_string (y, md) = match md with
| None -> Fmt.str "%04d" y
| Some (m, None) -> Fmt.str "%04d-%02d" y m
| Some (m, Some d) -> Fmt.str "%04d-%02d-%02d" y m d

let pp_partial ppf v = Fmt.string ppf (partial_to_string v)
