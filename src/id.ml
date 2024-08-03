(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
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
