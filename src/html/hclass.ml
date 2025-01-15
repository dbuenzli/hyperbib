(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std

let c = At.class'

let addable = c "addable"
let authors = c "authors"
let container = c "container"
let container_loc = c "container-loc"
let container_ids = c "container-ids"
let creatable = c "creatable"
let description = c "description"
let doi = c "doi"
let edit_pages = c "edit-pages"
let edit_ui = c "edit-ui"
let editing = c "editing"
let editors = c "editors"
let entity = c "entity"
let entity_kind = c "entity-kind"
let entity_menu = c "entity-menu"
let entity_ui = c "entity-ui"
let error = c "error"
let fake_hr_bottom = c "fake-hr-bottom"
let field = c "field"
let hyperbib = c "hyperbib"
let index = c "index"
let item_count = c "item-count"
let list = c "list"
let letter = c "letter"
let letter_index = c "letter-index"
let login = c "login"
let logout = c "logout"
let minus = c "minus"
let more_details = c "more-details"
let p404 = c "p404"
let person = c "person"
let private' = c "private"
let reference_ids = c "reference-ids"
let ref = c "ref"
let ref_item = c "ref-item"
let ref_list = c "ref-list"
let remove = c "remove"
let replace = c "replace"
let secondary = c "secondary"
let search = c "search"
let search_results = c "search-results"
let hui_remove = c "hui-remove"
let removable = c "removable"
let subject = c "subject"
let testing = c "testing"
let toc = c "toc"
let icon_button = c "icon-button"
let uppercase = c "uppercase"
let user_ui = c "user-ui"
let user_view = c "user-view"
let value = c "value"

let year = c "year"

(* Animate *)

let fade = c "fade"

(* Messages *)

let message = c "message"
let warn = c "warn"
let error = c "error"
let info = c "info"
let good = c "good"

(* Utility *)

let hspace_0  = c "hspace-0"
let hspace_0_125  = c "hspace-0-125"
let hspace_0_25  = c "hspace-0-25"
let hspace_0_5  = c "hspace-0-5"
let hspace  = c "hspace"
let hspace_2  = c "hspace-2"
let hspace_4  = c "hspace-4"
let hspace_8  = c "hspace-8"

let vspace_000  = c "vspace_000"
let vspace_0125 = c "vspace_0125"
let vspace_025  = c "vspace_025"
let vspace_050  = c "vspace_050"
let vspace_075  = c "vspace_075"
let vspace_100  = c "vspace_100"
let vspace_125  = c "vspace_125"
let vspace_150  = c "vspace_150"
let vspace_175  = c "vspace_175"
let vspace_200  = c "vspace_200"
let vspace_400  = c "vspace_400"
let vspace_800  = c "vspace_800"

module Font = struct
  let xx_small = c "font_xx_small"
  let x_small = c "font_x_small"
  let small = c "font_small"
  let normal = c "font_normal"
  let large = c "font_large"
  let x_large = c "font_x_large"
  let xx_large = c "font_xx_large"

  let w100 = c "font_w100"
  let w200 = c "font_w200"
  let w300 = c "font_w300"
  let w400 = c "font_w400"
  let w500 = c "font_w500"
  let w600 = c "font_w600"
  let w700 = c "font_w700"
  let w800 = c "font_w800"
  let w900 = c "font_w900"

  let w_light = w300
  let w_normal = w400
  let w_bold = w700
end

module Margin = struct
  let top_000 = c "margin_top_000"
  let top_0125 = c "margin_top_0125"
  let top_025 = c "margin_top_025"
  let top_050 = c "margin_top_050"
  let top_075 = c "margin_top_075"
  let top_100 = c "margin_top_100"
  let top_125 = c "margin_top_125"
  let top_150 = c "margin_top_150"
end

module Gap = struct
  let v_025 = c "gap_025"
  let v_050 = c "gap_050"
  let x_025 = c "gap_x_025"
  let x_050 = c "gap_x_050"
  let y_025 = c "gap_y_025"
  let y_050 = c "gap_y_050"
end
