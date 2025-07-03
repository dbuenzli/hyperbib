(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hyperbib standard needs. *)

(** {1:std Standard libray needs} *)

module Fmt = More.Fmt
module Fpath = More.Fpath
module Log = More.Log
module Cmd = More.Cmd
module Os = More.Os

(** {1:extended_stdlib Extended [Stdlib] modules} *)

module Char = More.Char
module String = More.String
module Bytes = Bytesrw.Bytes
module List = More.List
module Result = More.Result

(** {1:lib Library shortcuts} *)

module Url = Webs.Url
module Media_type = Webs.Media_type
module Http = Webs.Http
module Http_client = Webs.Http_client
module Res = Webs_bazaar.Res
module Kurl = Webs_bazaar.Kurl

module At = Htmlit.At
module El = Htmlit.El

(** {1:ideally Ideally not here} *)

module Bag = Rel_query.Bag

(** {1:other Other needs} *)

module Bazaar : sig

  val cp_dir_content :
    ?dotfiles:bool -> ?follow_symlinks:bool -> recurse:bool ->
    of_dir:Fpath.t -> inside_dir:Fpath.t -> unit -> (unit, string) result
  (** FIXME it these kinds of things should likely be added to
      B0_std *)

  (** {1:rel [rel] stuff} *)

  open Rel

  val col_values :
    ?ignore:'r Col.def list -> 'r Rel.Row.t -> 'r -> 'r Col.value list
    (** FIXME we can likely have more efficient stuff than going through
        this. *)
end
