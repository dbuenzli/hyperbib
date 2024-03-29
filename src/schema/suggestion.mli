(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Reference suggestions. *)

open Hyperbib.Std

type id = Id.t
(** The type for suggestion ids. These are allocated by the database. *)

type t
(** The type for suggestions. *)

val v :
  id:id -> timestamp:int -> doi:Doi.t -> suggestion:string -> comment:string ->
  email:string -> unit -> t
(** [v …] is a suggestion with given properties. See accessors for
    semantics. *)

val row : id -> int -> Doi.t -> string -> string -> string -> t
(** [row …] is unlabelled {!v}. *)

val new' : t
(** [new] is a new suggestion. *)

val id : t -> id
(** [id s] is the unique identifier of [s]. *)

val timestamp : t -> int
(** [timestamp s] is the time when the suggestion made it to the database. *)

val doi : t -> Doi.t
(** [doi s] is the DOI of the suggestion (empty string if none). *)

val suggestion : t -> string
(** [suggestion s] is the free form text of the suggestion. *)

val comment : t -> string
(** [comment s], if any, is a private comment about the suggestion. *)

val email : t -> string
(** [email s], if any, is a private email to contact the suggester. *)

(** {1:table Table and queries} *)

val id' : (t, id) Rel.Col.t
val timestamp' : (t, int) Rel.Col.t
val doi' : (t, Doi.t) Rel.Col.t
val suggestion' : (t, string) Rel.Col.t
val comment' : (t, string) Rel.Col.t
val email' : (t, string) Rel.Col.t
val table : t Rel.Table.t

include Entity.IDENTIFIABLE_WITH_QUERIES with type t := t and type id := id
(** @inline *)

val list : (t, Bag.unordered) Rel_query.Bag.t
val list_stmt : t Rel_sql.Stmt.t

val find_doi : string Rel_query.value -> (t, Bag.unordered) Bag.t

(** {1:urls Urls} *)

module Url : sig
  type t =
  | Create
  | Confirm_delete of id
  | Delete of id
  | Fill_in
  | Index
  | Page of { id : id; created : bool }
  | View_fields of id

  val kind : t Kurl.kind
  val v : t -> Kurl.t
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The hyperbib programmers

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
