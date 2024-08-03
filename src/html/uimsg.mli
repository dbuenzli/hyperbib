(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** User interface messages. *)

val about_your_suggestion : string
val author : string
val authors : string
val add_author : string
val add_container : string
val add_editor : string
val add_person : string
val add_subject : string
val applications : string
val cancel : string
val cited_by : string
val cited_by_descr : string
val cited_by_descr_zero : string
val cites : string
val cites_descr : string
val cites_descr_zero : string
val confirm_delete : string
val container : string
val container_deleted : string -> string
val container_contained : string -> string
val container_contained_zero : string -> string
val container_list_descr : string
val containers : string
val comment : string
val copy : string
val create : string
val create_duplicate : string
val create_label : string
val create_subject : string
val create_and_add_author : string
val create_and_add_editor : string
val create_and_add_person : string
val create_person : string
val create_container : string
val create_and_add_container : string
val create_reference : string
val date : string
val details : string
val delete : string
val deleted : string
val description : string
val document_in_bib : string -> string
val document_suggested : string -> string
val doi : string
val doi_not_found : string -> string
val doi_error : string
val doi_unspecified : string
val duplicate : string
val edit : string
val editor : string
val editor_abbr : string
val editors : string
val editors_abbr : string
val email : string
val your_email : string
val email_note : string
val error : string
val fill_in_form_with_doi : string
val fill_in_suggestion_with_doi : string
val fill_in : string
val fill_in_suggestion : string
val find_subject : string
val find_container : string
val find_person : string
val first_names : string
val full_text : string
val go_back_to_bib : string
val goto_kind_index : string -> string
val hello : string
val help : string
val here : string
val in' : string
val integrate : string
val integrate_suggestion : string
val isbn : string
val issn : string
val issue : string
val it_is_still_applied_to : string
val it_will_be_removed_from : string
val kind_not_found : string -> string
val kind_page_does_not_exist : string -> string
val labels : string
val last_name : string
val login : string
val login_descr : string
val login_error : string
val logout : string
val make_all_authors_public : string
val make_a_suggestion : string
val name : string
val need_a_doi_or_suggestion : string
val new_container : string
val new_label : string
val new_person : string
val new_reference : string
val new_subject : string
val new_suggestion_on : string
val no_parent : string
val not_implemented_501 : string
val not_implemented_501_descr : string
val not_found : string
val not_found_404 : string
val not_found_404_descr : string
val note : string
val no_pending_suggestions : string
val or_username : string
val orcid : string
val other : string
val optional : string
val pages : string
val parent : string
val password : string
val pending_suggestions : string
val pending_suggestions_descr : string
val person : string
val person_duplicate_will_be_added_to : string
val person_deleted : string -> string
val person_list_descr : string
val person_mentioned_descr : string -> string
val person_mentioned_descr_zero : string -> string
val persons : string
val private' : string
val private_and_optional : string
val private_note : string
val private_tip : string
val public : string
val public_tip : string
val publisher : string
val please_try_again_in_a_few_days : string
val really_delete_subject : string -> string
val really_delete_person : string -> string
val really_delete_container : string -> string
val really_delete_reference : string -> string
val really_delete_suggestion : string
val reference : string
val reference_deleted : string -> string
val reference_list_descr : string
val references : string
val references_anchor : string
val remove_author : string
val remove_container : string
val remove_editor : string
val remove_person : string
val remove_reference : string
val remove_subject : string
val replace : string
val replace_container : string
val replace_container_by : string -> string
val replace_person : string
val replace_person_by : string -> string
val replace_subject : string
val replace_subject_by : string -> string
val replacement_subject_will_be_applied_to : string
val replacement_person_will_be_added_to : string
val replacement_container_will_become_container_of : string
val save_container : string
val save_person : string
val save_reference : string
val save_subject : string
val server_error_5XX : string
val server_error_5XX_descr : string
val something_went_wrong_XXX : string
val something_went_wrong_XXX_descr : string
val someone_made_new_suggestion_here : string
val subject : string
val subject_applied_descr : string -> string
val subject_applied_descr_zero : string -> string
val subject_deleted : string -> string
val subject_duplicate_will_be_applied_to : string
val subjects : string
val submit_suggestion : string
val suggested_by : string
val suggestion : string
val suggestions : string
val suggestion_form_descr : string
val suggestions_too_many : string
val suggested_reference : string
val system_thinks_you_are_a_bot : string
val these_n_references : int -> string
val this_is_a_testing_site : string
val this_cannot_be_undone : string
val this_will_also_delete : string
val the_email_address_will_be_deleted : string
val title : string
val type' : string
val unauthorized_401 : string
val unauthorized_401_descr : string
val unknown : string
val unnamed : string
val untitled : string
val user : string
val undo_make_all_authors_public : string
val volume : string
val warning : string
val year : string
val year_index_descr : string
val year_page_order_descr : string
val year_page_order_descr_zero : string
val years : string
val your_suggestions_for_addition : string
val you_may_want_to_send_an_email : string
val to_notify_the_suggestion_was_treated : string
