(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The login page. *)


val login : Page.Gen.t -> msg:string -> goto:string option -> Page.t
val page : Page.Gen.t -> username:string -> Page.t
