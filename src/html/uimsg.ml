(*---------------------------------------------------------------------------
   Copyright (c) 2021 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib.Std

let about_your_suggestion = "About your suggestion"
let author = "Author"
let authors = "Authors"
let add_author = "Add author"
let add_container = "Add container"
let add_editor = "Add editor"
let add_person = "Add person"
let add_subject = "Add subject"
let applications = "Applications"
let cancel = "Cancel"
let cited_by = "Cited by"
let cited_by_descr = "Cited by these reference in the corpus, listed by decreasing publication date."
let cited_by_descr_zero = "No reference in the corpus is citing this reference."
let cites = "Cites"
let cites_descr = "Citations in the corpus, listed by decreasing publication date."
let cites_descr_zero = "This reference is not cited by other references in the corpus."
let confirm_delete = "Confirm delete"
let container = "Container"
let container_deleted s = Fmt.str "The container ‘%s’ was deleted." s
let containers = "Containers"
let container_contained s = Fmt.str "These references are contained in ‘%s’." s
let container_contained_zero s = Fmt.str "No references are contained in ‘%s’." s
let container_list_descr = "Containers of references found in the corpus."
let copy = "Copy"
let comment = "Comment"
let create = "Create"
let create_duplicate = "Create duplicate"
let create_label = "Create label"
let create_subject = "Create subject"
let create_and_add_author = "Create person and add author"
let create_and_add_editor = "Create person and add editor"
let create_and_add_person = "Create person and add"
let create_person = "Create person"
let create_container = "Create container"
let create_and_add_container = "Create container"
let create_reference = "Create reference"
let date = "Date"
let details = "Details"
let delete = "Delete"
let deleted = "Deleted"
let description = "Description"
let document_in_bib s =
  Fmt.str "Warning, the document identified by DOI ‘%s’ is already part of the
    bibliography" s

let document_suggested s =
  Fmt.str "Warning, the document identified by DOI ‘%s’ is already suggested" s

let doi = "DOI"
let doi_not_found s =
  Fmt.str "Sorry, the document identified by DOI ‘%s’ cannot be found." s
let doi_error = Fmt.str "Sorry, an unexpected error occured during DOI lookup."
let doi_unspecified = "No DOI specified in ‘DOI’ field."
let duplicate = "Duplicate"
let edit = "Edit"
let editor = "Editor"
let editors = "Editors"
let editor_abbr = "ed."
let editors_abbr = "eds."
let email = "Email"
let your_email = "Your email"
let email_note =
  "(optional, kept private, deleted after the suggestion is treated)"

let error = "Error"
let fill_in_form_with_doi = "Fill in form with a DOI"
let fill_in_suggestion_with_doi = "Fill in suggestion with a DOI"
let fill_in = "Fill in"
let fill_in_suggestion = "Fill in suggestion"
let find_subject = "Find subject"
let find_container = "Find container"
let find_person = "Find person"
let first_names = "First name(s)"
let full_text = "Full text"
let go_back_to_bib = "Go back to the bibliography."
let goto_kind_index k = Fmt.str "Go to the %s index." k
let here = "Here"
let help = "Help"
let hello = "Hello"
let in' = "In"
let issn = "ISSN"
let isbn = "ISBN"
let issue = "Issue"
let integrate = "Integrate"
let integrate_suggestion = "Integrate suggestion"
let it_is_still_applied_to = "It is still applied to"
let it_will_be_removed_from = "It will be removed from"
let kind_not_found k = Fmt.str "%s not found" k
let kind_page_does_not_exist k = Fmt.str "Sorry, this %s page does not exist." k
let labels = "Labels"
let last_name = "Last name"
let login = "Log in"
let login_descr = "Please log in to edit the bibliography."
let login_error = "Unknown user or wrong password. Please try again."
let logout = "Log out"
let make_all_authors_public = "Make all authors public"
let make_a_suggestion = "Make a suggestion"
let name = "Name"
let need_a_doi_or_suggestion =
  "You need to at least fill in the ‘DOI’ or ‘Suggested reference’ field."

let new_container = "New container"
let new_label = "New label"
let new_person = "New person"
let new_reference = "New reference"
let new_subject = "New subject"
let new_suggestion_on = "New suggestion on"
let no_parent = "No parent"
let not_implemented_501 = "Feature not implemented (501)"
let not_implemented_501_descr = "Sorry, this feature is not implemented yet."
let not_found = "Not found"
let not_found_404 = "Page not found"
let not_found_404_descr = "Sorry, this page does not exist."
let no_pending_suggestions = "There are no pending suggestions."
let note = "Note"
let other = "Other"
let optional = "(optional)"
let or_username = "or username"
let orcid = "ORCID"
let pages = "Pages"
let parent = "Parent"
let password = "Password"
let person = "Person"
let person_deleted s = Fmt.str "The person ‘%s’ was deleted." s
let person_list_descr = "Persons mentioned in the corpus in author or editor position."
let person_mentioned_descr s = Fmt.str "The person ‘%s’ is mentioned in author or editor position in these references." s
let person_mentioned_descr_zero s = Fmt.str "No reference mentions the person ‘%s’." s

let person_duplicate_will_be_added_to = "The new person duplicate will be added to"

let persons = "Persons"

let pending_suggestions = "Pending suggestions"
let pending_suggestions_descr =
  "These suggestions have already been made and are waiting for review."

let please_try_again_in_a_few_days = "Please try again in a few days."
let private' = "Private"
let private_and_optional = "(kept private and optional)"
let private_note = "Private note"
let private_tip = "Show the private view"
let public = "Public"
let public = "Public"
let public_tip = "Show the public view"
let publisher = "Publisher"
let really_delete_container s = Fmt.str "Do you really want to delete container ‘%s’ ?" s
let really_delete_subject s = Fmt.str "Do you really want to delete subject ‘%s’ ?" s
let really_delete_person s = Fmt.str "Do you really want to delete person ‘%s’ ?" s
let really_delete_reference s = Fmt.str "Do you really want to delete reference ‘%s’ ?" s

let really_delete_suggestion = "Do you really want to delete the suggestion"

let reference = "Reference"
let reference_list_descr = "All references in the corpus, listed by decreasing publication date."
let references = "References"
let references_anchor = "references"
let reference_deleted s = Fmt.str "The reference ‘%s’ was deleted." s
let remove_author = "Remove author"
let remove_container = "Remove container"
let remove_editor = "Remove editor"
let remove_reference = "Remove reference"
let remove_subject = "Remove subject"
let remove_person = "Remove person"
let replace = "Replace"
let replace_container = "Replace container"
let replace_container_by s = Fmt.str "Replace container ‘%s’ by:" s
let replace_person = "Replace person"
let replace_person_by s = Fmt.str "Replace person ‘%s’ by:" s
let replace_subject = "Replace subject"
let replace_subject_by s = Fmt.str "Replace subject ‘%s’ by:" s
let replacement_subject_will_be_applied_to = "The replacement subject will be applied to"
let replacement_person_will_be_added_to = "The replacement person will be added to"

let replacement_container_will_become_container_of = "The replacement container will become the container of"
let save_container = "Save container"
let save_person = "Save person"
let save_reference = "Save reference"
let save_subject = "Save subject"
let server_error_5XX = "Internal error"
let server_error_5XX_descr = Fmt.str "Sorry, an internal error occured."
let something_went_wrong_XXX = "Something went wrong"
let something_went_wrong_XXX_descr = "Sorry, something went wrong."
let someone_made_new_suggestion_here = "Someone made a new suggestion here:"
let subject = "Subject"
let subject_applied_descr s = Fmt.str "The subject ‘%s’ is applied to these references." s
let subject_applied_descr_zero s = Fmt.str "No reference has the subject ‘%s’." s
let subject_deleted s = Fmt.str "The subject ‘%s’ was deleted." s
let subject_duplicate_will_be_applied_to = "The new subject duplicate will be applied to"
let subjects = "Subjects"
let submit_suggestion = "Submit suggestion"
let suggestion = "Suggestion"
let suggestions = "Suggestions"
let suggested_by = "Suggested by"
let suggestion_form_descr =
  "To make a suggestion, use the form below. Use a DOI to fill in \
   the suggestion if you have one."

let suggestions_too_many =
  "Sorry! We are not accepting suggestions for the time being. We have \
   too many pending suggestions."

let suggested_reference = "Suggested reference"
let system_thinks_you_are_a_bot =
  "The system thinks you are a robot. If that is not the case please \
   accept your apologies and contact one of the humans responsible for \
   the bibliography."

let this_cannot_be_undone = "This cannot be undone."
let the_email_address_will_be_deleted =
  "The email address will be deleted from the system."

let this_will_also_delete = "This will also delete"
let this_is_a_testing_site = "This is a testing site, your changes are not preserved"
let title = "Title"
let type' = "Type"
let unauthorized_401 = "Not authorized"
let unauthorized_401_descr = "Sorry, you do not have appropriate permissions to access this page."
let unknown = "Unknown"
let unnamed = "Unnamed"
let untitled = "Untitled"
let user = "User"
let undo_make_all_authors_public = "Undo make all authors public"
let volume = "Volume"
let warning = "Warning"

let year = "Year"
let year_index_descr = "Classified by year of earliest, online or print, publication date."
let year_page_order_descr = "References published this year, ordered by earliest, online or print, publication date."
let year_page_order_descr_zero = "No references published this year."
let years = "Years"
let your_suggestions_for_addition = "Your suggestions for addition"
let you_may_want_to_send_an_email = "You may want to send an email to"
let to_notify_the_suggestion_was_treated =
  "to notify that the suggestion was treated."

let these_n_references = function
| 0 -> "no reference"
| 1 -> "this reference"
| n -> Fmt.str "these %d references" n
