(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Derive JSON types from OCaml type gists.

    The {!to_jsont} function derives a {!Jsont.t} value from a
    {!Typegist.Type.Gist.t} value.

    A few metadata keys in {!Meta} keys can be used to customize the
    computed JSON type or selectively override the deriving function
    with a manually provided {!Jsont.t} value.

    See an {{!example}example}. *)

open Typegist

(** Metadata for customizing {!to_jsont}.

    {b TODO} We need a special meta for fields so that
    we can have two parameters and specify a {!Jsont.Mem.map}. *)
module Meta : sig

  (** Specifies a JSON type to use instead of the derivation.

      See {!example}. *)

  (** {1:member Object members} *)

  (** If this key is specified on a product field. The definition
      is used of the field. *)
  module Mem_map : Type.Gist.Meta.KEY
    with type ('a, 'b) value := ('a, 'b) Jsont.Object.Mem.map

  (** If this key is specified and {!Mem_map} is not specified
      this is used for the [enc_omit] parameter of the member map. *)
  module Mem_enc_omit : Type.Gist.Meta.KEY
    with type ('o, 'a) value := 'a -> bool

  (** If this key is specified and {!Mem_map} is not specified
      this is used for the member name of the member map. *)
  module Mem_name : Type.Gist.Meta.KEY
    with type ('o, 'a) value := string

  (** If this key is specified on a type gist. The definition is
      used for the type. *)
  module Jsont : Type.Gist.Meta.KEY with type ('a, 'b) value := 'a Jsont.t
end

val to_jsont : 'a Type.Gist.t -> 'a Jsont.t
(** [to_jsont g] is a JSON type derived fom [g].

    In general:
    {ul
    {- {!Typegist.Type.Gist.type_name} is used for {!Jsont.kind}.}
    {- {!Typegist.Type.Gist.Meta.Doc} (if any) is used for {!Jsont.doc}.}}

    Note the following:
    {ul
    {- {!Typegist.Type.Gist.Float} uses {!Jsont.number} by default.}
    {- {!Typegist.Type.Gist.Int64} uses {!Jsont.int64} by default.}
    {- {!Typegist.Type.Gist.String} uses {!Jsont.string} by default.}
    {- {!Typegist.Type.Gist.String} uses a transformed
       {!Jsont.binary_string} by default.}}

    Raises [Invalid_argument] in the following cases:
    {ul
    {- {!Typegist.Type.Gist.Ignore} is specified on a field
       but the field has no {!Typegist.Type.Gist.Field.default}
       value.}}
*)


val gist : 'a Type.Gist.t -> 'a Jsont.t Type.Gist.t
(** [gist g] is a gist for JSON type values. This is not really usable
    in general but can be useful to inspect {!Jsont.t} values with
    {!Typegist.Fun.Generic.pp}. *)

(** {1:example Example}

    This example overrides the string key.
*)
