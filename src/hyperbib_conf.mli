(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** General [hyperbib] tool configuration. *)

type t
(** The type for configurations. *)

val make :
  app_dir:Fpath.t -> http_client:(Http_client.t, string) result -> unit -> t
(** [make] is a configuration with given atributes. See the acessors
    for semantics. *)

val app_dir : t -> Fpath.t
(** [app_dir c] is the absolute path to the application directory. *)

val users_file : t -> Fpath.t
(** [users_file c] is the JSON file holding application users. *)

val doi_cache_dir : t -> Fpath.t
(** [doic_cache_dir c] is the absolute path to the DOI metadata
      cache directory. *)

val static_dir : t -> Fpath.t
(** [static_dir c] is the absolute path to the static assets directory. *)

val db_file : t -> Fpath.t
(** [db_file c] is the sqlite3 database file holding application data. *)

val db_backup_file : t -> Fpath.t
(** [db_backup_file c] is the stable backup of the sqlite3 database file
      holding application data. *)

val blobstore_dir : t -> Fpath.t
(** [blobstore_dir c] is the path to the documentation store. *)

val blobstore : t -> (Blobstore.t, string) result
(** [blobstore c] is the blob store of {!blobstore_dir}. *)

  (*
  val bib_conf_file : t -> Fpath.t
  (** [bib_conf_file c] is the file holding the bibliography configuration. *)
*)

val authentication_private_key : t -> Fpath.t
(** [authentication_private_key c] is the file that stores the private key
      to authenticate data like session cookies. *)

val http_client : t -> (Http_client.t, string) result
(** [http_client c] is the HTTP client to use in the app. *)


val with_cli : app_dir:Hyperbib_std.Fpath.t option -> (t, string) result

val with_db : t -> (Db.t -> 'a) -> ('a, string) result

val with_db_transaction :
  t -> Db.transaction_kind ->
  (Db.t -> ('a, string) result) -> ('a, string) result
