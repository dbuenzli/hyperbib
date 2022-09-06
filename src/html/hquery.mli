(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTTP request query helpers *)

open Hyperbib.Std
open Rel


(* FIXME a less ad-hoc set of functions can be designed here. *)

(** {1:gen Generic} *)

(* FIXME not convient for form building. Should we separate kind from
   key name ? *)

type 'a kind
val kind : string -> (string -> 'a option) -> 'a kind
val bool : bool kind
(* FIXME HTML checkboxes don't work that way [find_col]
   does the right thing *)

val int : int kind
val string : string kind

type 'a key
val key : string -> 'a kind -> 'a key
val find : 'a key -> none:'a -> Http.query -> ('a, Http.resp) result
val find' : 'a key -> Http.query -> ('a option, Http.resp) result
val find_all : 'a key -> Http.query -> ('a list, Http.resp) result
val get : 'a key -> Http.query -> ('a, Http.resp) result
val get_all : 'a key -> Http.query -> ('a list, Http.resp) result


(* Get rid of that. *)

val uniquify_ids : int list -> int list


(** {1:rel Rel generic} *)

val find_col :
  ('r, 'a) Col.t -> none:'a -> Http.query -> ('a, Http.resp) result
(** [find_col col ~none q] looks up [col] in [q]. For [Type.Bool] columns
    [none] is never considered. Absence of the value is [false] with
    HTML checkboxes. *)

val get_col :
  ('r, 'a) Col.t -> Http.query -> ('a, Http.resp) result
(** [get_col] is like {!find_col} except this responds with
    {!Http.Resp.bad_request_400} if the column cannot be found, except
    for [Type.Bool] columns since absence of the values denotes
    [false] for HTML checkboxes. *)

val find_cols :
  cols:'r Col.v list -> Http.query -> ('r Col.value list, Http.resp) result

val find_table_cols :
  'r Table.t -> cols:'r Col.v list -> Http.query ->
  ('r Col.value list, Http.resp) result
(** [find_table_cols t cs] finds the value of columns [cs] of [t] in [q]. *)

val careless_find_table_cols :
  ?ignore:'r Col.v list -> 'r Table.t ->
  Http.query -> ('r Col.value list, Http.resp) result
(** Do not use, use {!find_table_cols}. [careless_find_table_cols r q]
    finds in [q] all column names of [t], except those mentioned in
    [ignore].

    {b WARNING.} It is not a good idea to use this function, when your
    database schema evolves, it may end up exposing columns to queries
    that weere not meant to be updatable by the query. *)





val key_for_rel : ?suff:string -> 'r Table.t -> ('r, 'a) Col.t -> string

(** {1:hyperbib Hyperbib specific} *)

val is_undo : string
val key_is_undo : bool key

val find_ids :
  uniquify:bool -> string -> Http.query -> (int list, Http.resp) result

val date_key : string
val find_date : Http.query -> (Date.partial option, string) result

val cite_key : string
val find_cites : Http.query -> Doi.t list

val person_key : Person.role option -> string

val create_container_title : string
val create_container_issn : string
val create_container_isbn : string
val find_create_container : Http.query -> Container.t option

val create_author_first : string
val create_author_last : string
val create_author_orcid : string
(* val find_create_authors : Http.query -> Person.t list *)

val create_editor_first : string
val create_editor_last : string
val create_editor_orcid : string


val find_create_person :
  public:bool -> role:Person.role option -> Http.query -> Person.t option

(* val find_create_editors : Http.query -> Person.t list *)

val create_person_keys : Person.role option -> string * string * string

val find_create_contributors :
  Http.query ->
   ([`Id of Person.id | `To_create of Person.t] list *
    [`Id of Person.id | `To_create of Person.t] list, Http.resp) result


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
