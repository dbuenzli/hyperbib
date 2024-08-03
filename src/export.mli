(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data export functions. *)

open Hyperbib.Std

val static_html :
  inside_dir:Fpath.t -> Hyperbib.Conf.t -> Db.t -> Page.Gen.t ->
  (unit, string) result

val bibtex_of_refs :
  now:Ptime.t -> Bibliography.t -> Reference.render_data ->
  (string, string) result
