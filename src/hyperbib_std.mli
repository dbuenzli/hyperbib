(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hyperbib standard needs. *)

(** {1:std Standard libray needs}

    For now we piggy back on B0_std but we should not. *)

module Char = B0_std.Char
module String = B0_std.String
module Result = B0_std.Result
module Fmt = B0_std.Fmt
module List = B0_std.List
module Fpath = B0_std.Fpath
module Log = B0_std.Log
module Cmd = B0_std.Cmd
module Os = B0_std.Os

(** {1:lib Library shortcuts} *)

module Url = Webs_url
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
    ?ignore:'r Col.v list -> 'r Rel.Row.t -> 'r -> 'r Col.value list
    (** FIXME we can likely have more efficient stuff than going through
        this. *)
end
