(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Database abstraction. *)

open Hyperbib_std

(** {1:db Database} *)

val dialect : Rel_sql.dialect
(** The the database SQL dialect. *)

type error
(** The type for database errors. *)

val error_message : error -> string
(** [error_message e] is an english error message for [e]. *)

val error_rc_message : error -> string

val string_error : ('a, error) result -> ('a, string) result
(** [string_error] is [Result.map_error error_message]. *)

type t
(** The type for database connections. *)

val with_open :
  ?foreign_keys:bool -> ?read_only:bool -> Fpath.t -> (t -> 'a) ->
  ('a, error) result
(** [with_open file yf] calls [f] with a database open on [file].  If
    [read_only] is [true] (defaults to [false]), no writes are
    allowed. This sets the database in WAL mode. See also
    {!with_open_schema}. *)

val ensure_db_path : Fpath.t -> (unit, string) result
(** [ensures_db_path db_file] the parent of [db_file] exists. It's a
    bit annoying to not have that as a [make_path] flag in
    [with_open], but we are not in the right error monad. *)

(** {1:pool Connection pool} *)

type pool = (t, error) Rel_pool.t
(** The type for database connection pools. *)

val pool : ?read_only:bool -> Fpath.t -> size:int -> pool
(** [pool file ~size] pools [size] connections on the database [file].
    If [read_only] is [true] (defaults to [false]), no writes are
    allowed. *)

(** {1:backups Backups} *)

val stamped_backup_file : Fpath.t -> Fpath.t
(** [stamped_backup_file file] appends a second precision local time stamp
    to the basename of [file]. *)

val backup : Fpath.t -> t -> (unit, string) result
(** [backup file db] backups [db] to [file]. [file] is replaced
    atomically. This means that with the original db file, we need as
    much a three times the space of the db. *)

val backup_thread : pool -> every_s:int -> Fpath.t -> Thread.t
(** [backup_thread p every_s file] makes every [every_s] seconds a
    stable copy to [file] of the database drawn from [p]. *)

(** {1:transactions Transaction} *)

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]

val with_transaction :
  transaction_kind ->  t ->
  (t -> ('a, 'b) result) -> (('a, 'b) result, error) result

(** {1:schema Schema handling} *)

val clear : t -> (unit, error) result
(** [reset db] drops all tables in [db]. *)

val ensure_schema :
  ?read_only:bool -> Rel.Schema.t -> t -> (unit, string) result
(** [ensure_schema schema db] ensure schema [schema] is in place i
    [db]. [read_only] indicates whether [db] is read-only, defaults to
    [false]. *)

val schema :
  ?schema:Rel.Schema.name -> t -> (Rel.Schema.t * string list, error) result
(** [schema db] is the schema of [db]. *)

val with_open_schema :
  ?foreign_keys:bool -> ?read_only:bool -> Rel.Schema.t ->
  Fpath.t -> (t -> 'a) -> ('a, string) result
(** [with_open_schema] is {!with_open} followed by {!ensure_schema}. *)

(** {1:queries Queries} *)

val exec_sql : t -> string -> (unit, error) result

val exec : t -> unit Rel_sql.Stmt.t -> (unit, error) result
(** [exec] abstracts {!Rel_sqlite3.exec}. *)

val first : t -> 'a Rel_sql.Stmt.t -> ('a option, error) result
(** [first] abstracts {!Rel_sqlite3.first}. *)

val fold : t ->
  'r Rel_sql.Stmt.t -> ('r -> 'c -> 'c) -> 'c -> ('c, error) result
(** [fold] abstracts {!Rel_sqlite3.fold}. *)

val list : t -> 'a Rel_sql.Stmt.t -> ('a list, error) result

val insert :
  t -> unit Rel_sql.Stmt.t -> (Id.t, error) result

val id_map :
  t -> 'a Rel_sql.Stmt.t -> ('a -> Id.t) -> ('a Id.Map.t, error) result

val id_map_related_list :
  ?order:('b -> 'b -> int) ->
  t -> 'a Rel_sql.Stmt.t -> id:('a -> int) -> related:('a -> int) ->
  related_by_id:'b Id.Map.t -> ('b list Id.Map.t, error) result

(** {1:debug Statement debug} *)

val show_sql : ?name:string -> 'a Rel_sql.Stmt.t -> 'a Rel_sql.Stmt.t
(** [show_sql ~name st] dumps the SQL of [st] on the program log and
    returns [st]. *)

val show_plan : ?name:string -> t -> 'a Rel_sql.Stmt.t -> 'a Rel_sql.Stmt.t
(** [explain_plan ~name db st] dumps a query plan explanation of [st]
    on the program log and returns [st]. *)

(** {1:webs Webs responses} *)

val http_resp_error :
  ?retry_after_s:int -> ('a, error) result -> ('a, Http.Response.t) result
(** [http_resp_error e] is a webs response for [Error e].
    {!Rel.Error.busy_time_out} errors are mapped to a
    {!Webs.Http.service_unavailable_503} with a {!Webs.Http.retry_after}
    header of [retry_after_s] seconds (default to [2]). *)

(** Same as {!queries} but composed with {!error_resp}. *)

val exec' : t -> unit Rel_sql.Stmt.t -> (unit, Webs.Http.Response.t) result
val insert' : t -> unit Rel_sql.Stmt.t -> (Id.t, Webs.Http.Response.t) result
val first' : t -> 'a Rel_sql.Stmt.t -> ('a option, Webs.Http.Response.t) result
val list' : t -> 'a Rel_sql.Stmt.t -> ('a list, Webs.Http.Response.t) result
val with_transaction' :
  transaction_kind ->
  t -> (t -> ('a, 'b) result) -> (('a, 'b) result, Webs.Http.Response.t) result
