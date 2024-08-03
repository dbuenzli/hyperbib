(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std


type t
val copyrights : t -> string
val project_title : t -> string
val project_short_title : t -> string
val project_href : t -> string
val favicon_href : t -> string option
val bibtex_filename : t -> string
val suggester_email_message : t -> string

val get : unit -> (t, string) result


module Url : sig
  type bibliography = t
  type t =
  | Home
  | Help
  | Bibtex_file of string

  val kind : t Kurl.kind
  val v : t -> Kurl.t
  val bibtex_file : bibliography -> Kurl.t
end
