(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Database abstraction. *)

open Hyperbib.Std

val dialect : Rel_sql.dialect
(** The the database SQL dialect. *)

type t
(** The type for the database. *)

type error
(** The type for database errors. *)

val error_message : error -> string
(** [error_message e] is an english error message for [e]. *)

val string_error : ('a, error) result -> ('a, string) result
(** [string_error] is [Result.map_error error_message]. *)

type pool = (t, error) Rel_pool.t
(** The type for database connection pools. *)

val pool : ?read_only:bool -> Fpath.t -> size:int -> pool
(** [pool file ~size] pools [size] connections on the database [file].
    If [read_only] is [true] (defaults to [false]), no writes are
    allowed. *)

val open' :
  ?foreign_keys:bool -> ?read_only:bool -> Fpath.t -> (t, error) result
(** [open' file] opens the database [file]. If [read_only] is [true]
    (defaults to [false]), no writes are allowed. *)

val close : t -> (unit, error) result
(** [close db] closes the database *)

val with_open : Fpath.t -> (t -> 'a) -> ('a, error) result
(** [with_open file f] calls [f] with a database open on [file]. *)

val setup :
  t -> schema:Rel.Schema.t -> (unit, error) result
(** [setup db] setups the database if needed. *)

val reset :
  t -> schema:Rel.Schema.t -> (unit, error) result

val backup : Fpath.t -> t -> (unit, string) result
(** [backup file db] backups [db] to [file]. [file] is replaced
    atomically. This means that with the original db file, we need as
    much a three times the space of the db. *)

val backup_thread : pool -> every_s:int -> Fpath.t -> Thread.t
(** [backup_thread p every_s file] makes every [every_s] seconds a
    stable copy to [file] of the database drawn from [p]. *)

(** {1:debug Debug convenience} *)

val show_sql : ?name:string -> 'a Sql.Stmt.t -> 'a Sql.Stmt.t
(** [show_sql ~name st] dumps the SQL of [st] on the program log and
    returns [st]. *)

val show_plan : ?name:string -> t -> 'a Sql.Stmt.t -> 'a Sql.Stmt.t
(** [explain_plan ~name db st] dumps a query plan explanation of [st]
    on the program log and returns [st]. *)

(** {1:queries Queries and convenience} *)

val exec : t -> unit Sql.Stmt.t -> (unit, error) result
val first : t -> 'a Sql.Stmt.t -> ('a option, error) result
val fold : t -> 'r Sql.Stmt.t -> ('r -> 'c -> 'c) -> 'c -> ('c, error) result
val list : t -> 'a Sql.Stmt.t -> ('a list, error) result

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]

val with_transaction :
  transaction_kind ->  t -> (t -> ('a, 'b) result) ->
  (('a, 'b) result, error) result

val insert : t -> unit Sql.Stmt.t -> (Id.t, error) result
val id_map : t -> 'a Sql.Stmt.t -> ('a -> Id.t) -> ('a Id.Map.t, error) result
val id_map_related_list :
  ?order:('b -> 'b -> int) ->
  t -> 'a Sql.Stmt.t -> id:('a -> int) -> related:('a -> int) ->
  related_by_id:'b Id.Map.t -> ('b list Id.Map.t, error) result

val schema :
  ?schema:Rel.Schema.name -> t -> (Rel.Schema.t * string list, error) result

(** {1:webs Webs responding convenience} *)

val error_resp :
  ?retry_after_s:int -> ('a, error) result -> ('a, Http.resp) result
(** [error_resp e] is a webs response for [Error e]. {!Rel.Error.busy_time_out}
    errors are mapped to a {!Webs.Http.service_unavailable_503} with
    a {!Webs.Http.retry_after} header of [retry_after_s] seconds (default
    to [2]). *)

(** Same as {!queries} but composed with {!error_resp}. *)

val exec' : t -> unit Sql.Stmt.t -> (unit, Webs.Http.resp) result
val insert' : t -> unit Sql.Stmt.t -> (Id.t, Webs.Http.resp) result
val first' : t -> 'a Sql.Stmt.t -> ('a option, Webs.Http.resp) result
val list' : t -> 'a Sql.Stmt.t -> ('a list, Webs.Http.resp) result
val with_transaction' :
  transaction_kind ->
  t -> (t -> ('a, 'b) result) -> (('a, 'b) result, Webs.Http.resp) result

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
