(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP request query helpers *)

open Hyperbib_std
open Rel


(* FIXME a less ad-hoc set of functions can be designed here. *)

(** {1:gen Generic} *)

type 'a kind
val kind : string -> (string -> 'a option) -> 'a kind
val bool : bool kind
(* FIXME HTML checkboxes don't work that way [find_col]
   does the right thing *)

val int : int kind
val string : string kind

type 'a key
val key : string -> 'a kind -> 'a key
val find_first :
  'a key -> none:'a -> Http.Query.t -> ('a, Http.Response.t) result

val find_first' : 'a key -> Http.Query.t -> ('a option, Http.Response.t) result
val find_all : 'a key -> Http.Query.t -> ('a list, Http.Response.t) result
val get : 'a key -> Http.Query.t -> ('a, Http.Response.t) result
val get_all : 'a key -> Http.Query.t -> ('a list, Http.Response.t) result


(* Get rid of that. *)
val uniquify_ids : (module Rel_kit.ID with type t = 'a) -> 'a list -> 'a list


(** {1:rel Rel generic} *)

val find_col :
  ('r, 'a) Col.t -> none:'a -> Http.Query.t -> ('a, Http.Response.t) result
(** [find_col col ~none q] looks up [col] in [q]. For [Type.Bool] columns
    [none] is never considered. Absence of the value is [false] with
    HTML checkboxes. *)

val get_col :
  ('r, 'a) Col.t -> Http.Query.t -> ('a, Http.Response.t) result
(** [get_col] is like {!find_col} except this responds with
    {!Webs.Http.Response.bad_request_400} if the column cannot be found, except
    for [Type.Bool] columns since absence of the values denotes
    [false] for HTML checkboxes. *)

val find_cols :
  cols:'r Col.def list -> Http.Query.t ->
  ('r Col.value list, Http.Response.t) result

val find_table_cols :
  'r Table.t -> cols:'r Col.def list -> Http.Query.t ->
  ('r Col.value list, Http.Response.t) result
(** [find_table_cols t cs] finds the value of columns [cs] of [t] in [q]. *)

val careless_find_table_cols :
  ?ignore:'r Col.def list -> 'r Table.t ->
  Http.Query.t -> ('r Col.value list, Http.Response.t) result
(** Do not use, use {!find_table_cols}. [careless_find_table_cols r q]
    finds in [q] all column names of [t], except those mentioned in
    [ignore].

    {b WARNING.} It is not a good idea to use this function, when your
    database schema evolves, it may end up exposing columns to queries
    that were not meant to be updatable by the query. *)


val key_for_rel : ?suff:string -> 'r Table.t -> ('r, 'a) Col.t -> string

(** {1:hyperbib Hyperbib specific} *)

val is_undo : string
val key_is_undo : bool key

val find_ids :
  (module Rel_kit.ID with type t = 'id) ->
  uniquify:bool -> string -> Http.Query.t ->
  ('id list, Http.Response.t) result

val date_key : string
val find_date : Http.Query.t -> (Date.partial option, string) result

val cite_key : string
val find_cites : Http.Query.t -> string list

val person_key : Person.Role.t option -> string

val create_container_title : string
val create_container_issn : string
val create_container_isbn : string
val find_create_container : Http.Query.t -> Container.t option

val create_author_first : string
val create_author_last : string
val create_author_orcid : string
(* val find_create_authors : Http.Query.t -> Person.t list *)

val create_editor_first : string
val create_editor_last : string
val create_editor_orcid : string


val find_create_person :
  public:bool -> role:Person.Role.t option -> Http.Query.t -> Person.t option

(* val find_create_editors : Http.Query.t -> Person.t list *)

val create_person_keys : Person.Role.t option -> string * string * string

val find_create_contributors :
  Http.Query.t ->
   ([`Id of Person.Id.t | `To_create of Person.t] list *
    [`Id of Person.Id.t | `To_create of Person.t] list, Http.Response.t) result
