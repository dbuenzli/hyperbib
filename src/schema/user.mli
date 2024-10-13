(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Application users. *)

open Hyperbib_std

(** {1:user User} *)

type t
(** The type for a single user. *)

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

val remove : name:string -> s -> s
(** [remove name us] is [us] without a user named [us]. *)

val check : name:string -> password:string -> s -> bool
(** [check ~name ~password us] is [true] iff [name] exists and its
    password matches [pass]. *)

val fold : (t -> 'a -> 'a) -> s -> 'a -> 'a
(** [fold f us] folds over the users of [us]. *)

(** {1:serial Serialiazing} *)

val jsont : s Jsont.t
(** [jsont] is a JSON serializer for users. A JSON array
    of objects of this form:
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

  val make : edit:bool -> t
  (** [make ~edit] are capabilities with given attributes. See
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
