(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Page render and content. *)

open Hyperbib.Std

(** {1:params Page rendering parameters} *)

(** HTML page generation parameters. *)
module Gen : sig

  type auth_ui = [ `Login | `Logout ]
  (** The type for authentication uis. *)

  type user_view = [ `Public | `Private ]
  (** The type for user views. *)

  type t
  (** The type for page generation parameters. Provides a few globals
      and determines the information shown on page according to the
      kind of user session or rendering mode (e.g. static generation). *)

  val v :
    now:Ptime.t -> Bibliography.t -> Kurl.fmt ->
    auth_ui:auth_ui option -> user_view:user_view option ->
    private_data:bool -> testing:bool -> t

  val now : t -> Ptime.t (* FIXME this should be last db update. *)
  val bibliography : t -> Bibliography.t

  val url_fmt : t -> Kurl.fmt

  val auth_ui : t -> auth_ui option
  (** [auth_ui l] is the authentication UI to show. *)

  val user_view : t -> user_view option
  (** [user_view g] is the UI view to show to users. *)

  val editable : t -> bool
  (** [editable g] is [true] if [edit_ui g] is [Some `Edit]. *)

  val only_public : t -> bool
  (** [only_public g] is the negation of [private_data]. *)

  val private_data : t -> bool
  (** [private_data g] is [true] if private data should be rendered. *)

  val testing : t -> bool
  (** [testing g] is [true] if the testing install banner should be rendered *)
end



(** {1:pages Pages} *)

type t
(** The type for a page. This is a page's HTML and its url. This structure
    only exists because of the static export mecanism. *)

val v : url:Kurl.t -> html:El.html -> t
(** [v ~url ~doc ~part] is a page for [url] with contents [doc]. *)

val url : t -> Kurl.t
(** [url p] is the url of [p]. *)

val html : t -> El.html
(** [html p] is HTML of [p]. *)

val with_content :
  ?ui_ext:(Gen.t -> self:Kurl.t -> El.html) ->
  Gen.t -> self:Kurl.t -> title:string -> content:El.html -> t
(** [with_content g ~self ~title ~content ~ui_ext] is a base hyperbib
    page.  [ui_ext] allows for page specific user interface
    extensions. *)

val for_404 :
  ?ui_ext:(Gen.t -> self:Kurl.t -> El.html) ->
  Gen.t -> kind:string -> self:Kurl.t -> consult:Kurl.t -> t
(** [for_404 g ~kind ~self ~consult] is a 404 page for URL [self]
    identifing a resource of kind [kind] enticing to look at [consult]
    instead. *)

val full_title : Gen.t -> title:string -> string
(** [full_title] is used by {!with_content} to generate a title. *)

(** {1:resp Respones} *)

val response :
  ?explain:string -> ?reason:string -> ?headers:Http.Headers.t -> ?status:int ->
  t -> Http.Response.t
(** [resp p] is an HTML page response for [p]. *)

val part_response :
  ?explain:string -> ?reason:string -> ?headers:Http.Headers.t -> ?status:int ->
  El.html -> Http.Response.t
(** [part_response html] an HTML part response for [html] *)

val response_404 :
  ?explain:string -> ?reason:string -> ?headers:Http.Headers.t -> t ->
  Http.Response.t
(** [resp_404] is [resp ~status:Http.not_found_404]. *)

(** {1:errors Errors} *)

val error : Gen.t -> Http.Request.t -> Http.Response.t -> Http.Response.t
(** [error g request response] is a generic HTML error page when [request]
    errors with [response]. *)

(** {1:fragments Fragment} *)

val doi_resolver : El.html
(** [doi_resolver] is a fragment for the client DOI resolution. *)

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
