(*---------------------------------------------------------------------------
   Copyright (c) 2025 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** ORCIDs.

    See the {{:https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier}
    structure of the ORCID identifier}.
*)

(** {1:orcids ORCIDs} *)

type t
(** The type for ORCIDs. *)

val v : string -> t
(** [v s] is [s] as an ORCID. Raises {!Invalid_argument} if it doesn't
    parse as an ORCID. Use {!of_string} to deal with untrusted inputs. *)

(** {1:converting Converting} *)

(**/**)
val unsafe_of_string : string -> t
(**/**)

val of_string : string -> (t, string) result
(** [of_string s] parses and validates the checksum of an ORCID
    [s]. The ORCID can start with [https://orcid.org/], [http://orcid.org/]
    or have no URL prefix. *)

val to_string : t -> string
(** [to_string id] is [id] as a string without the URL prefix. See also
    {{!presentations}presentations}. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an ORCID like {!to_string} does. *)

val jsont : t Jsont.t
(** [jsont] parses JSON strings into ORCIDs using {!to_string}.
    Encoding uses the {!as_https_url} presentation. *)

(** {1:presentations Presentations} *)

val as_http_url : t -> string
(** [as_http_url id] expresses [id] with the [http] prefix. *)

val as_https_url : t -> string
(** [as_https_url id] expresses [id] with the [https] prefix. *)

val as_filename : t -> string
(** [as_filename id] is an string suitable as a filename. This is the
    identity. *)

(** {1:predicates Predictes and comparisons} *)

val equal : t -> t -> bool
(** [equal] is binary equality on ORCIDs. *)

val compare : t -> t -> int
(** [compare] is a total order on ORCIDs compatible with {!equal}. *)

(** {1:setmap Sets and maps} *)

module Set : Set.S
(** Sets of ORCIDs *)

module Map : Map.S
(** Maps of ORCIDs *)
