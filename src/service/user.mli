(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Application users. *)

open Hyperbib.Std
open B00_serialk_json

(** 1:user User} *)

type t

val name : t -> string
(** [name u] is the name of [u]. *)

(** {1:users Users} *)

type s
(** The type for users. *)

val empty : s
(** [empty] has no users. *)

val is_empty : s -> bool
(** [is_empty us] is [true] iff [us] has no users. *)

val mem : name:string -> s -> bool
(** [mem ~name us] is [true] if a user named [name] exists. *)

val add : name:string -> password:string -> s -> s
(** [add ~name ~password us] adds a user named [name] with password [pass]
    to [us]. If [name] already exists this override the previous
    definition. *)

val check : name:string -> password:string -> s -> bool
(** [check ~name ~password us] is [true] iff [name] exists and its
    password matches [pass]. *)

val fold : (t -> 'a -> 'a) -> s -> 'a -> 'a
(** [fold f us] folds over the users of [us]. *)

(** {1:serial Serialiazing} *)

val of_json : s B00_serialk_json.Jsonq.t
(** [of_json] queries a JSON array of objects of the form
{v
   {
     "username": "name",
     "password":
     {
        "algo": "pbkdf2-hmac-sha-256",
        "iterations" : 100000,
        "salt" : "ffffffff",
        "key" : "fffffffff"
     }
   }
v} *)

val to_json : s -> Jsong.t
(** [to_json us] serializes users [us] to JSON. *)

(** {1:persist Persisting} *)

val load : Fpath.t -> (s, string) result
(** [load file] are users from the JSON file [file]. This is {!empty} if
    the file does not exist. *)

val save : Fpath.t -> s -> (unit, string) result
(** [save file us] saves users us to the JSON file [file]. *)

(** {1:caps Capabilities} *)

(** User capabilities. *)
module Caps : sig

  type t
  (** The type for user capabilities. *)

  val v : edit:bool -> t
  (** [v ~edit] are capabilities with given attributes. See
      corresponding accessors for semantics. *)

  val edit : t -> bool
  (** [edit cs] is true if [cs] has the edit capability. This
      allows to edit anything on the webapp. *)

  val none : t
  (** [none] has no capability. *)
end

(** {1:url Urls} *)

module Url : sig
  type goto = string option
  val goto_key : string
  val username_key : string
  val password_key : string

  type t =
  | Login of { goto : goto }
  | Authenticate of { goto : goto }
  | Logout of { goto : goto }
  | View of { private' : bool }

  val kind : t Kurl.kind
  val v : t -> Kurl.t
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
