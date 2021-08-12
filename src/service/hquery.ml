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
