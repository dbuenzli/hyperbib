# Code layout 

## Cli tool and backend

All the source is in [`src`](src). This directory has a few generic
modules independent from the project, the commands of the tool, and it
ties up the web service definition from the components in
[src/service](src/service).

The [src/service](src/service) directory has the datastructure
definitions, their HTML rendering and their services which are
eventually aggregated in the final URL request tree by
[`src/service.ml`](src/service.ml). It also has the page generation
mecanism and a few more generic helper modules which could eventually
be a bit generalized and maybe librarificated. 

In general given a data model entity, for example a `Person.t`. The following
modules are defined:

* `src/service/person.ml[i]`. Defines the OCaml datatype, it's representation
  as a table, related database queries and operations and the definition of 
  a `Kurl` URL request kind for it in `Person.Url`.
* `src/service/person_html.ml[i]`. This has HTML rendering fragments for 
  handling the datatype. In general this should be pure function from 
  data, no database or network access here.
* `src/service/person_service.ml[i]`. Service implementation for the 
  request kind defined by `Person.Url.t`. This reads and writes the database, 
  and implements the HTTP churn to respond to requests.

This layered structure has a few advantages for module dependencies:

1. `Person_html` can access the whole model and the URLs requests
   to format.
2. `Person_service` can access the whole model and all the HTML renderings.

Since we have highly hyperlinked data we still occasionaly run by
accident into the annoying circular dependency.

**FIXME** Right now we have a bit of data gather logic duplication `src/service/*_service.ml` files and in [`src/export.ml`](src/export.ml).  This should be fixed.

## Front end

The front end support is in [src/front](src/front). This has the fonts
and `.css` files and the front end code. The OCaml files are compiled
to JavaScript via `js_of_ocaml` and *for now* everything is copied
over by the build system to [`app/static`](app/static).

For now there's very little code there. Beyond the generic `hc`
support we just have the logic for changing DOI URLs on the page
according to a user defined resolver stored on its browser and a few
generic tools to work around the deficiencies of the HTML native UI
widgets.

The goal is to have as little as possible here and push the `hc` way.
A few more things could be removed if we improve it, e.g. with local
DOM tree substitutions.

# Web service

The webservice uses the HTTP/1.1 `Webs` connector, each connection is
served by a single thread, if needed (most of the time for now) the
thread draws a database connection from a pool. Both threads and
database connections are pooled with the same size.

# Database use and schema

Database use is abstracted by the [`src/db.mli`](src/db.mli) module, there 
should be no mention of the `Ask_sqlite3` anywhere else.

## Usage

The SQLite database holds the bibliographic data and metadata. It does
not hold user data (e.g. username or passwords). That way the file can
be directly as a research artifact for preservation (the SQLite file
format is recommended by the [Library of congress][loc]).

[loc]: https://www.loc.gov/preservation/resources/rfs/data.html

## Schema

The SQL database definition can be output via 

```
b0 -a hyperbib -- db schema
```

To visualize the schema diagram use 
```
b0 -a hyperbib -- db diagram | dot -Tsvg > /tmp/hyperbib.svg \
  && show-uri /tmp/hyperbib.svg
```










