(*---------------------------------------------------------------------------
   Copyright (c) 2025 The hyperbib programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

(** General [hyperbib] tool configuration. *)

(** {1:config Configuration} *)

type t
(** The type for configurations. *)

val make :
  data_dir:Fpath.t -> cache_dir:Fpath.t ->
  http_client:(Http.Client.t, string) result -> unit -> t
(** [make] is a configuration with given atributes. See the acessors
    for semantics. *)

val discover :
  data_dir:Fpath.t option -> cache_dir:Fpath.t option -> (t, string) result
(** [discover] is a configuration whose unspecified attributes
    are discovered. *)

(** {1:props Properties} *)

val data_dir : t -> Fpath.t
(** [data_dir c] is the absolute path to the app's data directory. *)

val cache_dir : t -> Fpath.t
(** [cache_dir c] is the absolute path to the app's cache directory. *)

val users_file : t -> Fpath.t
(** [users_file c] is the JSON file holding app's users. *)

val doi_cache_dir : t -> Fpath.t
(** [doic_cache_dir c] is the absolute path to the DOI metadata
    cache directory (this is in {!cache_dir}) *)

val static_dir : t -> Fpath.t
(** [static_dir c] is the absolute path to the static assets directory. *)

val db_file : t -> Fpath.t
(** [db_file c] is the sqlite3 database file holding application data. *)

val db_backup_file : t -> Fpath.t
(** [db_backup_file c] is the stable backup of the sqlite3 database file
    holding application data. *)

val blobstore_dir : t -> Fpath.t
(** [blobstore_dir c] is the absolute path to the documentation store. *)

val blobstore : t -> (Blobstore.t, string) result
(** [blobstore c] is the blob store of {!blobstore_dir}. *)

  (*
  val bib_conf_file : t -> Fpath.t
  (** [bib_conf_file c] is the file holding the bibliography configuration. *)
*)

val authentication_secret_key_file : t -> Fpath.t
(** [authentication_secret_key_file c] is absolute path to the file that stores
    the secret key to authenticate session cookies. *)

val http_client : t -> (Http.Client.t, string) result
(** [http_client c] is the HTTP client to use in the app. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats configurations for inspection. *)

(** {1:db Using the database} *)

val with_db : t -> (Db.t -> 'a) -> ('a, string) result

val with_db_transaction :
  t -> Db.transaction_kind -> (Db.t -> ('a, string) result) ->
  ('a, string) result
