(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Entity HTML commonalities. *)

open Hyperbib_std

(** {1:description_field Description field} *)

val edit_description :
  (module Entity.DESCRIBABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_description :
  (module Entity.DESCRIBABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

(** {1:note_field Note field} *)

val edit_note :
  (module Entity.ANNOTABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_note :
  (module Entity.ANNOTABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

(** {1:private_note Private note field} *)

val edit_private_note :
  (module Entity.PRIVATELY_ANNOTABLE with type t = 't) ->
  ?textarea_at:At.t list -> ?at:At.t list -> 't -> El.html

val view_private_note :
  (module Entity.PRIVATELY_ANNOTABLE with type t = 't) -> Page.Gen.t ->
  ?at:At.t list -> 't -> El.html

(** {1:public Public field} *)

val edit_public :
  (module Entity.PUBLICABLE with type t = 't) ->
  ?at:At.t list -> 't -> El.html

val viz : (module Entity.PUBLICABLE with type t = 't) -> 't -> At.t
(** [viz (module E) e] is {!At.void} when [e] is public and
    {!Hclass.private'} otherwise. *)

(** {1:person_inputs Person inputs} *)

(** {1:contributors Contributor inputs} *)

val person_input :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  role:Person.role option -> Person.t -> El.html
(** [person_input uf ~for_list ~input_name ~role p] inputs [p] via a hidden
    field named [input_name] and whose name is [s]'s [id]. If [for_list] is
    [true] this is for a list of subjects; if [false] it's for selecting a
    single person. *)

val person_input_create :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  role:Person.role option -> Person.t -> El.html
(** [person_input_create uf ~input_name p] inputs [p] for creation
    via hidden fields listed for now in {!Hquery}. [input_name] is
    used in case the creation is removed, to replace it with a
    {!person_input_finder}.  *)

val person_input_finder :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  role:Person.role option -> El.html
(** [person_input_finder] is a text field to search for a person to input
    in an hidden input element named [input_name]. If [for_list] is [true]
    this is for a list of persons; if [false] it's for selecting a single
    subject. *)

val person_input_finder_results :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  role:Person.role option -> creatable:Person.t option -> Person.t list ->
  El.html
(** [person_input_finder_results] is a list of persons to choose from
    to input a person in a hidden input element named [input_name].
    If [creatable] is provided the given person can be created. *)

(** {1:subject_inputs Subject inputs} *)

val subject_input :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  Subject.t -> El.html
(** [subject_input uf ~for_list ~input_name s] inputs [s] via a hidden field
    named [input_name] and whose name is [s]'s [id]. If [for_list] is [true]
    this is for a list of subjects; if [false] it's for selecting a single
    subject. *)

val subject_input_finder :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name -> El.html
(** [subject_input_finder] is a text field to search for a subjec to input
    in an hidden input element named [input_name]. If [for_list] is [true]
    this is for a list of subjects; if [false] it's for selecting a single
    subject. *)

val subject_input_finder_results :
  Kurl.fmt -> for_list:bool -> input_name:Entity.Url.input_name ->
  parents:Subject.t Subject.Id.Map.t -> Subject.t list -> El.html
(** [subject_input_finder_results] is a list of subjects to choose from
    to input a subject in a hidden input element named [input_name].
    If [creatable] is provided the given subject can be created. *)

(** {1:container_inputs Container inputs} *)

val container_input :
  Kurl.fmt -> input_name:Entity.Url.input_name -> Container.t -> El.html
(** [container_input uf ~input_name c] inputs [c] via a hidden field
    named [input_name] and whose value is [c]'s [id]. *)

val container_input_create :
  Kurl.fmt -> input_name:Entity.Url.input_name -> Container.t -> El.html
(** [container_input_create uf ~input_name c] inputs [c] for creation
    via hidden fields listed for now in {!Hquery}. [input_name] is
    used in case the creation is removed, to replace it with a
    {!container_input_finder}. *)

val container_input_finder :
  Kurl.fmt -> input_name:Entity.Url.input_name -> El.html
(** [input_container] is a text field to search for a container to input
    in an hidden input element named [input_name]. *)

val container_input_finder_results :
  Kurl.fmt -> input_name:Entity.Url.input_name ->
  creatable:Container.t option -> Container.t list -> El.html
(** [container_input_finder_results] is a list of containers to choose from
    to input a container in a hidden input element named [input_name].
    If [creatable] is provided the given container can be created. *)
