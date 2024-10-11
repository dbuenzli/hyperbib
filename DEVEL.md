# Testing 

Given a test database in `app/` the following invocation makes it easy 
to test the webapp in any browser on http://localhost:8000/ 

```
b0 -- hyperbib serve --edit=unsafe --insecure-cookie
```

Or using the B0 `.show-url` action to reload your browser:

```
b0 -- .show-url http://localhost:8000/persons -- hyperbib serve …
```

There are also a few component oriented tests that can be invoked with: 

```
b0 test
```

# Code layout 

All the source is in [`src`](src):

* The directory has a few project pervasives modules and a few generic
  modules independent from the project.
  
* [`src/tool`](src/tool) has the commands and implementation of the 
  `hyperbib` tool.

* [`src/schema`](src/schema), has the database schemas 
  their HTML rendering in [`src/html`](src/html)
  and their service in [`src/service`](src/service) which are 
  eventually aggregated in the final URL request tree by
  [`src/service_tree.ml`](src/service_tree.ml).

* [`src/html`](src/html) also has the page generation mecanism and a few
  more generic helper modules which could eventually be a bit
  generalized and maybe libraryficated.

* [`src/front`](src/front) has the front end support see below.

In general given a schema entity, for example a `Person.t`. The following
modules are defined:

* `src/schema/person.ml[i]`. Defines the OCaml datatype, it's representation
  as a table, related database queries and operations and the definition of 
  a `Kurl` URL request kind for it in `Person.Url`.
* `src/html/person_html.ml[i]`. This has HTML rendering fragments for 
  handling the datatype. In general this should be pure function from 
  data, no database or network access here.
* `src/service/person_service.ml[i]`. Service implementation for the 
  request kind defined by `Person.Url.t`. This reads and writes the database, 
  and implements the HTTP churn to respond to requests.

This layered structure has a few advantages for module dependencies:

1. `Person_html` can access the whole schema and the URLs requests
   to format.
2. `Person_service` can access the whole schema and all the HTML renderings.

Since we have highly hyperlinked data we still occasionaly run by
accident into the annoying circular dependency.

**FIXME** Right now we have a bit of data gather logic duplication
`src/service/*_service.ml` files and in
[`src/export.ml`](src/export.ml).  This should be fixed.

# Database and schema

The SQLite database holds the bibliographic data and metadata. It does
not hold user data (e.g. username or passwords). That way the file can
be directly as a research artifact for preservation (the SQLite file
format is recommended by the [Library of congress][loc]).

Database use is abstracted by the [`src/db.mli`](src/db.mli) module, there 
should be (almost) no mention of the `Rel_sqlite3` anywhere else.

The SQL database definition can be output via 

```
b0 -- hyperbib db schema app
```

To visualize the schema diagram use 
```
b0 -- hyperbib db schema -f dot app | dot -Tsvg | show-url -t s.svg
```

# Document storage 

Documents associated to bibiographic records are stored on the file
system by using the [`src/blobstorage.mli`](src/blobstorage.mli)
abstraction which stores blobs indexed by their XXH3-128 hash. The
metadata is stored in the database in the
[`src/schema/doc.mli`](src/schema/doc.mli) table.

# Web service

The webservice uses the HTTP/1.1 `Webs` connector, each connection is
served by a single thread, if needed (most of the time for now) the
thread draws a database connection from a pool. Both threads and
database connections are pooled with the same size.

# Front end

The front end support is in [`src/front`](src/front). This has the fonts
and `.css` files and the front end code. The OCaml files are compiled
to JavaScript via `js_of_ocaml` and *for now* everything is copied
over by the build system to [`app/static`](app/static).

For now there's very little code there. Beyond the generic `htmlact`
support we just have the logic for changing DOI URLs on the page
according to a user defined resolver stored on its browser and a few
generic tools to work around the deficiencies of the HTML native UI
widgets.

The goal is to have as little as possible here and push the `htmlact`
way.  A few more things could be removed if we improve it, e.g. with
local DOM tree substitutions.




[loc]: https://www.loc.gov/preservation/resources/rfs/data.html













