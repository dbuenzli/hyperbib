(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type t = int

module T = struct
  type nonrec t = t
  let compare i0 i1 = (compare : int -> int -> int) i0 i1
end
module Set = Set.Make (T)
module Map = struct
  include Map.Make (T)

  let add_to_list k v m = match find_opt k m with
  | None -> add k [v] m
  | Some l -> add k (v :: l) m

  let add_to_set
      (type set) (type elt)
      (module S : Stdlib.Set.S with type elt = elt and type t = set)
      k v m
    =
    match find_opt k m with
    | None -> add k (S.singleton v) m
    | Some set -> add k (S.add v set) m

  let get_list k m = match find_opt k m with None -> [] | Some l -> l

  let of_list id l = List.fold_left (fun acc v -> add (id v) v acc) empty l
end
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
