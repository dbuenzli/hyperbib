(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Legacy data model and rendering *)

open Hyperbib.Std

module Container' : sig
  val id : Legacy.Container'.t -> string
  val href : Kurl.fmt -> id:string -> string
  val name : Legacy.Container'.t -> El.html

  val link : Kurl.fmt -> string -> El.html
  val page :
    ref_list_html:(Kurl.fmt -> Legacy.Refdb.t -> Legacy.Reference'.t list ->
                   El.html)
    -> Page.Gen.t -> Legacy.Refdb.t -> Page.t
end

module Subject' : sig
  val href : Kurl.fmt -> id:string -> string
  val id : Legacy.Subject'.t -> string
  val page :
    ref_list_html:(Kurl.fmt -> Legacy.Refdb.t -> Legacy.Reference'.t list ->
                   El.html)
    -> Page.Gen.t -> Legacy.Refdb.t -> Page.t
end

module Person' : sig
  val id : Legacy.Person'.t -> string
  val href : Kurl.fmt -> id:string -> string
  val names : Legacy.Person'.t -> El.html
  val link : Kurl.fmt -> Legacy.Person'.t -> El.html
  val inline_list : Kurl.fmt -> Legacy.Person'.t list -> El.html
  val page :
    ref_list_html:(Kurl.fmt -> Legacy.Refdb.t -> Legacy.Reference'.t list ->
                   El.html)
    -> Page.Gen.t -> Legacy.Refdb.t -> Page.t
end

module Year' : sig
  val href : Kurl.fmt -> id:string -> string
  val id : int -> string

  val page :
    ref_list_html:(Kurl.fmt -> Legacy.Refdb.t -> Legacy.Reference'.t list ->
                   El.html) ->
    Page.Gen.t -> Legacy.Refdb.t -> Page.t
end

module Reference' : sig
  val href : Kurl.fmt -> id:string -> string
  val id : Legacy.Reference'.t -> string
  val item : Kurl.fmt -> Legacy.Refdb.t -> Legacy.Reference'.t -> El.html
  val list_by_desc_date : Kurl.fmt -> Legacy.Refdb.t ->
    Legacy.Reference'.t list -> El.html

  val page : Page.Gen.t -> Legacy.Refdb.t -> Page.t
end


val help_page : Page.Gen.t -> Legacy.Refdb.t -> Page.t
val home_page : Page.Gen.t -> Legacy.Refdb.t -> Page.t

val gen_html :
  dir:Fpath.t -> Page.Gen.t -> Legacy.Refdb.t -> (unit, string) result

(*---------------------------------------------------------------------------
   Copyright (c) 2020 University of Bern

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
