(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Database entities model commonalities. *)

open Hyperbib.Std

(** {1:sigs Entity signatures} *)

(** The type for identifiable entities. *)
module type IDENTIFIABLE = sig

  type id = Id.t
  (** The type for entity identifiers. *)

  type t
  (** The type for entities. *)

  val id : t -> id
  (** [id e] the identifier of entity [e]. *)

  val id' : (t, id) Col.t
  (** [id'] is the column for {!val-id}. *)

  val table : t Table.t
  (** [table] is the table of entities. *)
end

(** The type for publishable entities.

    An identifiable entity with a {!PUBLICABLE.public} column. *)
module type PUBLICABLE = sig

  include IDENTIFIABLE

  val public : t -> bool
  (** [public e] is the entity's publication status. *)

  val public' : (t, bool) Col.t
  (** [public'] is the column for {!val-public}. *)
end

(** The type for named entities. *)
module type NAMED = sig
  include PUBLICABLE

  val name : t -> string
  (** [name e] is the entity's names. *)

  val name' : (t, string) Col.t
  (** [name'] is the column for {!val-name}. *)
end

(** The type for annotable entities. *)
module type ANNOTABLE = sig
  include PUBLICABLE

  val note : t -> string
  (** [note e] is the entity's public note. *)

  val note' : (t, string) Col.t
  (** [note'] is the column for {!val-note}. *)
end

(** The type for privately annotable entities. *)
module type PRIVATELY_ANNOTABLE = sig
  include PUBLICABLE

  val private_note : t -> string
  (** [note e] is the entity's private note. *)

  val private_note' : (t, string) Col.t
  (** [private_note'] is the column for {!val-private_note}. *)
end

(** The type for describable entities.. *)
module type DESCRIBABLE = sig
  include PUBLICABLE

  val description : t -> string
  (** [description e] is the entity's public description. *)

  val description' : (t, string) Col.t
  (** [description'] is the column for {!val-description}. *)
end

(** {1:queries Entity queries} *)

(** The type for basic queries on identifiable entities. *)
module type IDENTIFIABLE_QUERIES = sig
  type id = Id.t
  (** The type for entity identifiers. *)

  type t
  (** The type for entities. *)

  val create : ignore_id:bool -> t -> unit Sql.Stmt.t
  (** [create ~ignore_id:bool v] creates a row for [v]. If
      [ignore_id] is [true] the identifier of [v] is ignored
      and one is allocated by the dbms. *)

  val create_cols : ignore_id:bool -> t Col.value list -> unit Sql.Stmt.t
  (** [create_cols] is like {!create} but uses the given column
      values instead. *)

  val delete : id -> unit Sql.Stmt.t
  (** [delete id] delete the row identified by [id]. *)

  val update : id -> t Col.value list -> unit Sql.Stmt.t
  (** [update id cols] updates the columsn [cols] of row [id]. *)

  val find_id : id Ask.value -> (t, Bag.unordered) Bag.t
  (** [find_id id] is the row identified by [id]. *)

  val find_id_stmt : id -> t Sql.Stmt.t
  (** [find_id_stmt id] is a statment for {!find_id}. *)

  val find_ids : (id, 'a) Bag.t -> (t, Bag.unordered) Bag.t
  (** [find_ids ids] are the rows identified by [ids]. *)
end

(** The type for identifiable entities and their queries. *)
module type IDENTIFIABLE_WITH_QUERIES = sig
  include IDENTIFIABLE
  include IDENTIFIABLE_QUERIES with type t := t and type id = id
end

(** Functor for identifiable queries *)
module Identifiable_queries (E : IDENTIFIABLE) : IDENTIFIABLE_QUERIES
  with type t := E.t and type id := E.id


(** The type for basic queries on publishable entities. *)
module type PUBLICABLE_QUERIES = sig
  include IDENTIFIABLE_QUERIES

  val list : only_public:bool Ask.value -> (t, Ask.Bag.unordered) Ask.Bag.t
  (** [list ~only_public] are the rows of the table or only the public
      ones if [only_public] is [true]. *)

  val list_stmt : only_public:bool -> t Sql.Stmt.t
  (** [list_stmt] is a statement for {!list}. *)
end

(** The type for publicable entities and their queries. *)
module type PUBLICABLE_WITH_QUERIES = sig
  include PUBLICABLE
  include PUBLICABLE_QUERIES with type t := t and type id = id
end

(** Functor for pubicable queries. *)
module Publicable_queries (E : PUBLICABLE) : PUBLICABLE_QUERIES
  with type t := E.t and type id := E.id

(** {1:urls Entity URLs} *)

(** Entity URL commalities. *)
module Url : sig


  (** {1:query Query keys} *)

  val replace_by : string
  val replace_by_of_query : Http.query -> (Id.t, Http.resp) result
  val replace_by_of_query' : Http.query -> (Id.t option , Http.resp) result

  type cancel_url = string option
  val cancel_url_of_query : Http.query -> cancel_url
  val cancel_url_to_query : cancel_url -> Http.query option

  val select : string
  val select_of_query : Http.query -> string
  val select_to_query : string -> Http.query option

  type input_name = string
  val input_name_of_query : Http.query -> (input_name, Http.resp) result
  val input_name_to_query : ?init:Http.query -> input_name -> Http.query

  type for_list = bool
  val for_list_of_query : Http.query -> (for_list, Http.resp) result
  val for_list_to_query : ?init:Http.query -> for_list -> Http.query

  (** {1:meth Methods} *)

  val meth_id :
    Kurl.bare -> 'a Http.Meth.constraint' list -> string ->
    ('a * Res.Id.t, Http.resp) result

  val get_id : Kurl.bare -> string -> ([> `GET ] * Res.Id.t, Http.resp) result
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
