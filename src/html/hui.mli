(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hui programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTML UI fragments.

    For now this is nothing more than classified markup. Sometimes
    the markup may be conditional on arguments.

    {b TODO}
    {ul
    {- Articulate a strategy for styling, for now the idea
       should be that styling should mostly be done on classes
       rather than elements.}
    {- Tool tips.}
    {- Regularize markup nesting structure in edit and non-edit mode.}}
 *)

open Webs
open Htmlit

(** Classes. *)
module Class : sig

  val for_col : ('a, 'b) Rel.Col.t -> At.t
  val for_table : 'a Rel.Table.t -> At.t

  val text : At.t
  val button : At.t
  val cancel : At.t
  val delete : At.t
  val finder : At.t
  val finder_input : At.t
  val finder_result : At.t
  val orderable : At.t
  val ordering : At.t
  val input : At.t
  val label : At.t
  val string : At.t
  val select : At.t
  val spinner : At.t
  val submit : At.t
  val text : At.t

  (** {1:sizes Size control} *)

  (* We should do something like this based
     on CSS absolute font sizes. *)

  val tiny : At.t

  (** {1:layout Layout control} *)

  val group : At.t
  val dir_h : At.t
  val dir_v : At.t
  val align_center : At.t
  val align_distribute : At.t
  val align_end : At.t
  val align_justify : At.t
  val align_start : At.t
  val align_stretch : At.t
  val x_align_center : At.t
  val x_align_distribute : At.t
  val x_align_end : At.t
  val x_align_justify : At.t
  val x_align_start : At.t
  val x_align_stretch : At.t
end

(** {1:group Groups}

    {b FIXME.} This was ripped of {!Brr_note_kit.Ui}.
    Once we should seriously absorb {{:https://www.w3.org/TR/css-align/}
    the box alignment module} and
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Grid_Layout/Box_Alignment_in_CSS_Grid_Layout}this} and try to devise a good system. *)

type dir =
[ `H (** Consecutive items are placed on the same line. *)
| `V (** Consecutive items are placed on new lines. *)  ]
(** The type for layout direction. *)

type align =
[ `Start (** Aligned at the start of the container. *)
| `End (** Aligned at the end of the container. *)
| `Center (** Centered in the container. *)
| `Justify (** Justified in the container. *)
| `Distribute (** Evenly distributed in the container. *)
| `Stretch (** TODO unclear and dodgy, in contrast to the others
                 this affects the size of items themselves. *) ]
(** The type for element alignement. *)

val group :
  ?at:At.t list -> ?x_align:align -> ?align:align -> dir:dir ->
  El.html list -> El.html
(** [group ~x_align ~align ~dir is] layouts items [cs] in a container.
    Arguments are as follows:
    {ul
    {- [dir] is the layout direction for elements}
    {- [align] is the alignement for items in the layout direction,
       defaults [`Start].}
    {- [x_align] is the alignment betwen elements in the direction
       perpendicular (the [x] for a cross) to the layout direction,
      defaults to [`Start].}} *)

(** {1:buttons Buttons} *)

val button :
  ?at:At.t list -> ?x_align:align -> ?align:align -> ?dir:dir ->
  ?tip:string -> ?type':string -> El.html -> El.html
(** [button ~at ~tip label] is a button with label [label] (which
    is [El.span]end, additional attributes [~at] and a tooltip [tip].
    FIXME talk about spinner *)

val button_link :
  ?at:At.t list -> ?x_align:align -> ?align:align -> ?dir:dir ->
  ?tip:string -> href:string -> El.html -> El.html
(** [button_link] is a button that goes to [href]. *)

val submit :
  ?at:At.t list -> ?x_align:align -> ?align:align -> ?dir:dir ->
  ?tip:string -> El.html -> El.html
(** [submit] is like {!button} but the HTML button type is [submit]
    and it is classified by [submit]. *)

val cancel :
  ?at:At.t list -> ?x_align:align -> ?align:align -> ?dir:dir ->
  ?tip:string -> El.html -> El.html
(** [cancel] is a {!button} but classified by [cancel]. *)

val delete :
  ?at:At.t list -> ?x_align:align -> ?align:align -> ?dir:dir ->
  ?tip:string -> El.html -> El.html
(** [delete] is a {!button} but classified by [delete]. *)

(** {1:input Input}

    {b TODO.} Make sure all Ask column names can be
    used as HTTP query key, see also {!Hquery} *)

val input_string' :
  ?at:At.t list -> ?autocomplete:bool -> ?autogrow:bool -> min_size:int ->
  name:string -> string -> El.html
(** [autcomplete] is a mess this is [true] by default
    and adds the [autocomplete='off'] attribute but that doesn't
    work in most browser. *)

val input_text' :
  ?at:At.t list ->  ?autogrow:bool -> min_rows:int -> name:string ->
  string -> El.html

val input_bool' : ?at:At.t list -> name:string -> bool -> El.html

val input_bool : ?at:At.t list -> col:('a, bool) Rel.Col.t -> 'a -> El.html

val input_string :
  ?at:At.t list -> ?autocomplete:bool -> ?autogrow:bool -> min_size:int ->
  col:('a, string) Rel.Col.t -> 'a -> El.html

val input_text :
  ?at:At.t list -> ?autogrow:bool -> min_rows:int ->
  col:('a, string) Rel.Col.t -> 'a -> El.html

val input_select :
  ?at:At.t list -> option_text:('a -> string) -> option_value:('a -> string) ->
  options:'a list -> col:('r, 'a) Rel.Col.t -> 'r -> El.html

val field_bool :
  ?input_at:At.t list -> ?at:At.t list -> label:El.html ->
  col:('r, bool) Rel.Col.t -> 'r -> El.html

val field_string' :
  ?input_at:At.t list -> ?at:At.t list -> ?autocomplete:bool ->
  ?autogrow:bool -> min_size:int -> label:El.html ->
  name:string -> string -> El.html

val field_string :
  ?input_at:At.t list -> ?at:At.t list -> ?autocomplete:bool ->
  ?autogrow:bool -> min_size:int -> label:El.html ->
  col:('r, string) Rel.Col.t -> 'r -> El.html

val field_text :
  ?textarea_at:At.t list -> ?at:At.t list -> ?autogrow:bool -> min_rows:int ->
  label:El.html -> col:('r, string) Rel.Col.t -> 'r -> El.html

(* XXX a version of field_text that takes a Col.value would be useful *)


val field_select :
  ?select_at:At.t list -> ?at:At.t list -> label:El.html ->
  option_text:('a -> string) -> option_value:('a -> string) ->
  options:'a list -> col:('r, 'a) Rel.Col.t -> 'r -> El.html
