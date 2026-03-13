(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** References by year pages. *)

open Hyperbib_std

val filter :
  year:Date.year option Rel_query.value -> (Reference.t, 'a) Bag.t ->
  (Reference.t, Bag.unordered) Bag.t
(** [filter ~year refs] are the reference of [refs] which have [year]. *)

val public_domain_stmt : (Date.year option * int) Rel_sql.Stmt.t


module Url : sig
  type t =
  | Index
  | Page of Date.year option

  val kind : t Kurl.kind
  val v : t -> Kurl.t
  val page : Date.year option -> Kurl.t
end
