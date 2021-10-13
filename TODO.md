
# Next step

* Escape in search fields.
* Double click to edit.


# Bug 

* Subject public when parent is non-public.


#  UI
* Slow/offline request feedbacks/disable multiple requests (hc?).
* Make nice things on blank states
* Contributor in place edit.
* Accesibility of custom drop down. Keyboard + activation by return.
  Escape -> clear 
  Look into https://github.com/alphagov/accessible-autocomplete
  No clear button in FF.
  Remove dropdown when no longer in focus.
* Preserve user input when things go wrong. 
* Audit, last update, undo
* Concurrent edit conflict scheme 

# Later
* Labels
* Typed database ids ! Rookie mistake.
* Get rid of the data collection duplication in services and export.
* Subject parent/replace, think more about parent/children relationship. 
* Put bibliography info in the db.
* Put pages data in the db.
* Markdown support in notes.
* Markdown local linking scheme.
* Subjects support for see also and see.
* Make nice things on blank states.
* Preserve user input when things go wrong.
* If we keep db id mononontic we could do redirects on replace operations.
* Comparison with doi data interface xs
* Cites cited-by ui is confusing.
* Use referer for ~self on fragments.
* Cleanups & bulk actions
* Public visiblity. This should likely be thought out more thoroughly. 
  E.g. should we should likely hide the references of a private container.
* Search, records and full text.
* We still have inter module unchecked dependencies. I suspect Kurl falls 
  short. We need to push towards full request deconstruction to custom 
  types. But the custom type may need to distinguish between specifying
  the end point (to be used by URL formatters) and the request data which 
  we don't have since it's created by the form.
