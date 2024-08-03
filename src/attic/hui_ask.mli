(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hui programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTML UI fragments for database. *)

open Webs
open Webs_html
open Ask.Std

(** {1:cols Rendering columns} *)

val th : ?name:(string -> El.html) -> ('r, 'a) Col.t -> El.html
(** [th c] is a table header for column [c] whose name is rendered
    with [name (Col.name c)] (defaults to {!El.txt}).  *)

val td : ?data:('a -> El.html) -> ('r, 'a) Col.t -> 'r -> El.html
(** [td c r] is table data for column [c] of row [r] whose data
    is rendered with [data] (defaults to {!El.txt} with {!Col.value_pp}). *)

(** {1:tables Rendering tables} *)

val tr_header : ?ignore:'r Col.v list -> 'r Table.t -> El.html
(** [tr_header ~ignore t] is a row of column headers for table [t].
    Columns in [ignore] are not rendered. *)

val tr_data : ?ignore:'r Col.v list -> 'r Table.t -> ('r -> El.html)
(** [tr ~ignore t] is a row renderer, using {!td} on each column of [t]
    except those listed in [ignore] (defaults to [[]]). *)

val table : ?ignore:'r Col.v list -> 'r Table.t -> 'r list -> El.html
(** [table ~ignore t rows] is a table for table [t] and rows [rows].
    Columns in [ignore] are not rendered (defaults to [[]]). *)

(** {1:user UI} *)

val text_field :
  ?at:At.t list -> ?autocomplete:bool -> edit:bool ->
  col:('a, string) Ask.Col.t -> label:string -> 'a -> El.html

val checkbox :
  ?at:At.t list -> ?value:string -> edit:bool -> col:('a, bool) Ask.Col.t ->
  label:string -> 'a -> El.html
