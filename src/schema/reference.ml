(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Hyperbib_std
open Rel

(* XXX Date modelling. Nothing really fits to get easy and efficient
   per year queries and year domain. I still think the most elegant
   would be two ints as unix timestamps to represent a range, but
   getting the year index and counts efficiently is unclear (my SQL is
   poor), here we just need an index on date_year and a GROUP BY.

   At least look in to table constraint support so that we model
   Partial_date.t in the db that is date_year IS NULL => data_md is
   NULL. Maybe something like

   ADD CONSTRAINT date
   CHECK (NOT (date_year IS NULL) OR (data_md is NULL))

   Bonus point if we can check the date is valid. *)

module Reference = struct
  module Id = Rel_kit.Id.MakeInt ()
  type t =
    { id : Id.t;
      abstract : string;
      container : Container.Id.t option;
      created_ptime_s : float;
      date : Date.partial option;
      doi : Doi.t option;
      isbn : string;
      issue : string;
      note : string;
      pages : string;
      private_note : string;
      public : bool;
      publisher : string;
      title : string;
      type' : string;
      volume : string; }

  let make
      ~id ~abstract ~container ~created_ptime_s ~date ~doi ~isbn ~issue ~note
      ~pages ~private_note ~public ~publisher ~title ~type' ~volume
    =
    { id; abstract; container; created_ptime_s; date; doi; isbn; issue; note;
      pages; private_note; public; publisher; title; type'; volume; }

  let row
      id abstract container created_ptime_s date_year date_md doi isbn issue
      note pages private_note public publisher title type' volume
    =
    let date = match date_year with
    | None ->
        (* N.B. we are dropping date_md if it existed see comment above. *)
        None
    | Some y -> Some (y, date_md)
    in
    { id; abstract; container; created_ptime_s; date; doi; isbn; issue; note;
      pages; private_note; public; publisher; title; type'; volume; }

  let new' =
    { id = Id.zero; abstract = ""; container = None; created_ptime_s = 0.;
      date = None; doi = None; isbn = ""; issue = ""; note = ""; pages = "";
      private_note = ""; public = false; publisher = ""; title = Uimsg.untitled;
      type' = ""; volume = ""; }

  let id r = r.id
  let abstract r = r.abstract
  let container r = r.container
  let created_ptime_s r = r.created_ptime_s
  let date r = r.date
  let doi r = r.doi
  let isbn r = r.isbn
  let issue r = r.issue
  let type' r = r.type'
  let note r = r.note
  let pages r = r.pages
  let private_note r = r.private_note
  let public r = r.public
  let publisher r = r.publisher
  let title r = r.title
  let volume r = r.volume

  (* Derived  *)

  let is_monograph_part r = match r.type' with
  (* Consider moving away from crossrefs types ? *)
  | "book-chapter" | "book-part" | "book-section"
  | "proceedings-article" -> true
  | _ -> false

  let date_year r = Option.map fst r.date
  let date_md r = Option.join (Option.map snd r.date)
  let year r = match r.date with None -> 0 | Some (y, _) -> y
  let non_empty_title r = match r.title with "" -> Uimsg.untitled | t -> t
  let compare_by_date r0 r1 = compare r0.date r1.date

  (* Table *)

  let id' = Col.make "id" Id.type' id
  let abstract' = Col.make "abstract" Type.text abstract
  let container' =
    Col.make "container" Type.(option Container.Id.type') container

  let created_ptime_s' =
    let default = `Value 0. in
    Col.make "created_ptime_s" Type.float created_ptime_s ~default

  let date_year' = Col.make "date_year" Type.(option int) date_year
  let date_md' =
    Col.make "date_md" Type.(option Schema_kit.Date_md_partial_rel.t) date_md

  let doi' = Col.make "doi" Type.(option Schema_kit.Doi_rel.t) doi
  let isbn' = Col.make "isbn" Type.text isbn
  let issue' = Col.make "issue" Type.text issue
  let note' = Col.make "note" Type.text note
  let pages' = Col.make "pages" Type.text pages
  let private_note' = Col.make "private_note" Type.text private_note
  let public' = Col.make "public" Type.bool public
  let publisher' = Col.make "publisher" Type.text publisher
  let title' = Col.make "title" Type.text title
  let type'' = Col.make "type" Type.text type'
  let volume' = Col.make "volume" Type.text volume
  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    let foreign_keys =
      [ Table.Foreign_key.make
          ~cols:[Def container']
          ~parent:(Table (Container.table, [Def Container.id']))
          ~on_delete:`Set_null () ]
    in
    let indices =
      [ Table.Index.make [Def doi'];
        Table.Index.make [Def container'];
        Table.Index.make [Def date_year']; ]
    in
    Table.make "reference" ~primary_key ~foreign_keys ~indices @@
    Row.(unit row * id' * abstract' * container' * created_ptime_s' *
         date_year' * date_md' * doi' * isbn' * issue' * note' * pages' *
         private_note' * public' * publisher' * title' * type'' * volume')


  let col_values_for_date = function
  | None -> Col.Value (date_year', None), Col.Value (date_md', None)
  | Some (y, None) ->
      Col.Value (date_year', (Some y)), Col.Value (date_md', None)
  | Some (y, (Some md)) ->
      Col.Value (date_year', (Some y)), Col.Value (date_md', (Some md))
end

include Reference

(* These abreviations are needed because OCaml lacks ../ references *)

module Reference_id = Id
type reference_id = Reference_id.t
type reference = t
let reference_id' = id'
let reference_table = table

type label = Label.t
let label_table = Label.table
let label_id = Label.id
let label_id_map = Label.id_map
let label_id_map_find_opt = Label.Id.Map.find_opt

type subject_id = Subject.Id.t
type subject = Subject.t
let subject_id_type = Subject.Id.type'
let subject_id_map = Subject.id_map
let subject_id_map_find_opt = Subject.Id.Map.find_opt
let subject_table = Subject.table
let subject_id = Subject.id
let subject_order_by_name = Subject.order_by_name

(* Relations *)

let label_order_by_name = Label.order_by_name
module Label = Label.For_entity (Reference)

module Contributor = struct
  type t =
    { reference : Id.t;
      person : Person.Id.t;
      role : Person.Role.t;
      position : int }

  let make ~reference ~person ~role ~position =
    { reference; person; role; position }

  let row reference person role position =
    { reference; person; role; position }

  let reference c = c.reference
  let person c = c.person
  let role c = c.role
  let position c = c.position
  let order_by_position c0 c1 = Int.compare c0.position c1.position

  let reference' = Col.make "reference" Id.type' reference
  let person' = Col.make "person" Person.Id.type' person
  let role' = Col.make "role" Person.Role.type' role
  let position' = Col.make "position" Type.int position
  let table =
    let primary_key =
      Table.Primary_key.make [Def reference'; Def person'; Def role'];
    in
    let foreign_keys =
      [ Table.Foreign_key.make
          ~cols:[Def reference']
          ~parent:(Table (table, [Def id']))
          ~on_delete:`Cascade ();
        Table.Foreign_key.make
          ~cols:[Def person']
          ~parent:(Table (Person.table, [Def Person.id']))
          ~on_delete:`Cascade () ]
    in
    let indices =
      [ Table.Index.make [Def person'];
        Table.Index.make [Def reference']]
    in
    Table.make "reference_contributor" ~primary_key ~foreign_keys ~indices
    @@ Row.(unit row * reference' * person' * role' * position')

  open Rel_query.Syntax

  let create ?or_action c = Rel_sql.insert_into Db.dialect ?or_action table c
  let of_ref_ids rids =
    let* rid = rids in
    let* c = Bag.table table in
    Bag.where Id.(rid = c #. reference') (Bag.yield c)

  let persons ~only_public cs =
    let* p = Bag.table Person.table in
    let is_used =
      Bag.exists @@
      let* c = cs in
      let is_used = Person.Id.(c #. person' = p #. Person.id') in
      let filter = Bool.(not only_public || p #. Person.public') in
      Bag.where Bool.(is_used && filter) (Bag.yield Bool.true')
    in
    Bag.where is_used (Bag.yield p)

  let copy_contributions_stmt =
    (* FIXME rel *)
    let sql =
      "INSERT OR REPLACE INTO reference_contributor
         (reference, person, role, position)
       SELECT c.reference, ?1, c.role, c.position
       FROM reference_contributor as c
       WHERE c.person = ?2"
    in
    let stmt =
      Rel_sql.Stmt.(func sql @@ Person.Id.type' @-> Person.Id.type' @-> unit)
    in
    fun ~src ~dst -> stmt dst src

  let set_list ~reference:id ~authors ~editors = fun db ->
    (* We could diff to devise delete and insert ops, for now it seems
       easier this way. *)
    let ref_col = Col.Value (reference', id) in
    let delete_all id = Rel_sql.delete_from Db.dialect table ~where:[ref_col] in
    let contributor role i pid =
      make ~reference:id ~person:pid ~role ~position:i
    in
    let authors = List.mapi (contributor Author) authors in
    let editors = List.mapi (contributor Editor) editors in
    let insert c = Db.exec db @@ create c in
    let open Result.Syntax in
    let* () = Db.exec db (delete_all id) in
    let* () = List.iter_stop_on_error insert authors in
    let* () = List.iter_stop_on_error insert editors in
    Ok ()
end

module Subject = struct
  type t =
    { reference : Id.t;
      subject : Subject.Id.t }

  let make ~reference ~subject = { reference; subject }
  let row reference subject = { reference; subject }
  let reference a = a.reference
  let subject a = a.subject

  let reference' = Col.make "reference" Id.type' reference
  let subject' = Col.make "subject" Subject.Id.type' subject
  let table =
    let primary_key = Table.Primary_key.make [Def reference'; Def subject'] in
    let foreign_keys = [
      Table.Foreign_key.make
        ~cols:[Def reference']
        ~parent:(Table (table, [Def id']))
        ~on_delete:`Cascade ();
      Table.Foreign_key.make
        ~cols:[Def subject']
        ~parent:(Table (Subject.table, [Def Subject.id']))
        ~on_delete:`Cascade () ]
    in
    let indices = [ Table.Index.make [Def subject'] ] in
    Table.make "reference_subject" ~primary_key ~foreign_keys ~indices @@
    Row.(unit row * reference' * subject')

  open Rel_query.Syntax

  let create ?or_action a = Rel_sql.insert_into Db.dialect ?or_action table a
  let of_ref_ids rids =
    let* rid = rids in
    let* rel = Bag.table table in
    Bag.where Id.(rid = rel #. reference') (Bag.yield rel)

  let subjects ~only_public rs =
    let* s = Bag.table Subject.table in
    let is_used =
      Bag.exists @@
      let* r = rs in
      let is_used = Subject.Id.(r #. subject' = s #. Subject.id') in
      let filter = Bool.(not only_public || s #. Subject.public') in
      Bag.where Bool.(is_used && filter) (Bag.yield Bool.true')
    in
    Bag.where is_used (Bag.yield s)

  let filter ~subjects refs =
    (* FIXME Rel make that work if subjects is a constant yield *)
    let* subj = subjects in
    let* ref = refs in
    let* app = Bag.table table in
    let has_subj =
      Subject.Id.(subj #. Subject.id' = app #. subject') &&
      Id.(ref #. Reference.id' = app #. reference')
    in
    Bag.where has_subj (Bag.yield ref)

  let filter_subject_id sid refs =
    let* app = Bag.table table in
    let* ref = refs in
    let has_subj =
      Subject.Id.(app #. subject' = sid) &&
      Reference.Id.(app #. reference' = ref #. Reference.id')
    in
    Bag.where has_subj (Bag.yield ref)

  let ref_count_stmt =
    (* FIXME rel aggregations. *)
    let sql =
      "SELECT COUNT(*)
       FROM reference_subject as s
       WHERE s.subject = ?1"
    in
    Rel_sql.Stmt.(func sql @@
                  Subject.Id.type' @-> ret Row.(t1 @@ int "ref_count"))

  let copy_applications_stmt =
    (* FIXME rel this is insert 'r table with 'r Bag.t *)
    let sql =
      "INSERT OR REPLACE INTO reference_subject (reference, subject)
       SELECT s.reference, ?1
       FROM reference_subject as s
       WHERE s.subject = ?2"
    in
    let stmt =
      Rel_sql.Stmt.(func sql @@ Subject.Id.type' @-> Subject.Id.type' @-> unit)
    in
    fun ~src ~dst -> stmt dst src

  let of_ref_id id =
    let* app = Bag.table table in
    Bag.where Id.(app #. reference' = id) (Bag.yield app)

  let ref_id_stmt =
    Rel_query.Sql.(func @@ Id.type' @-> ret (Table.row table) of_ref_id)

  let set_list ~reference:id ss = fun db ->
    (* We could diff to devise delete and insert ops, for now it seems
       easier this way. *)
    let ref_col = Col.Value (reference', id) in
    let delete_all id = Rel_sql.delete_from Db.dialect table ~where:[ref_col] in
    let insert sid =
      Db.exec db @@
      Rel_sql.insert_into_cols Db.dialect table
        [ref_col; Col.Value (subject', sid)]
    in
    let open Result.Syntax in
    let* () = Db.exec db (delete_all id) in
    let* () = List.iter_stop_on_error insert ss in
    Ok ()

end

module Cites = struct
  let reference_doi' = doi'

  type t = { reference : Id.t; doi : Doi.t }
  let make ~reference ~doi = { reference; doi }
  let row reference doi = { reference; doi }
  let reference c = c.reference
  let doi c = c.doi

  let reference' = Col.make "reference" Id.type' reference
  let doi' = Col.make "doi" Schema_kit.Doi_rel.t doi
  let table =
    let primary_key = Table.Primary_key.make [Def reference'; Def doi'] in
    let foreign_keys =
      [ Table.Foreign_key.make
          ~cols:[Def reference']
          ~parent:(Table (reference_table, [Def id']))
          ~on_delete:`Cascade (); ]
    in
    Table.make "cites" ~primary_key ~foreign_keys @@
    Row.(unit row * reference' * doi')

  open Rel_query.Syntax
  open Schema_kit

  let create c = Rel_sql.insert_into Db.dialect table c
  let of_ref_ids rids =
    let* rid = rids in
    let* rel = Bag.table table in
    Bag.where Id.(rid = rel #. reference') (Bag.yield rel)

  let internal_of_ref_ids rids =
    let* rid = rids in
    let* rel = Bag.table table in
    let* ref = Bag.table reference_table in
    let is_rid = Id.(rid = rel #. reference') in
    let is_internal_doi =
      Option.is_some (ref #. reference_doi') &&
      Doi_rel.(rel #. doi' = Option.get (ref #. reference_doi'))
    in
    let pair x y = Bag.inj (fun x y -> x, y) $ x $ y in (* FIXME rel *)
    let rel' = pair (rel #. reference') (ref #. id') in
    Bag.where (is_rid && is_internal_doi) (Bag.yield rel')

(*  let internal_row = Row.(t2 (int "ref") (int "cited")) *)
  let set_list ~reference:id ~dois = fun db ->
    (* We could diff to devise delete and insert ops, for now it seems
       easier this way. *)
    let ref_col = Col.Value (reference', id) in
    let delete_all id = Rel_sql.delete_from Db.dialect table ~where:[ref_col] in
    let cite doi = make ~reference:id ~doi in
    let cites = List.map cite dois in
    let insert c = Db.exec db @@ create c in
    let open Result.Syntax in
    let* () = Db.exec db (delete_all id) in
    let* () = List.iter_stop_on_error insert cites in
    Ok ()
end

module Doc = struct
  module Doc = struct
    module Id = Rel_kit.Id.MakeInt ()
    type t =
      { id : Id.t;
        reference : reference_id;
        blob_key : Blobstore.Key.text;
        media_type : Media_type.t;
        name : string;
        origin : string;
        public : bool; }

    let make ~id ~reference ~blob_key ~media_type ~name ~origin ~public =
      { id; reference; blob_key; media_type; name; origin; public }

    let row id reference blob_key media_type name origin public =
      { id; reference; blob_key; media_type; name; origin; public }

    let id d = d.id
    let reference d = d.reference
    let blob_key d = d.blob_key
    let media_type d = d.media_type
    let name d = d.name
    let origin d = d.origin
    let public d = d.public

    let order_by_media_type b0 b1 = String.compare b0.media_type b1.media_type

    open Rel

    let id' = Col.make "id" Id.type' id
    let reference' = Col.make "reference" Reference_id.type' reference
    let blob_key' = Col.make "blob_key" Type.text blob_key
    let media_type' = Col.make "media_type" Type.text media_type
    let name' = Col.make "name" Type.text name
    let origin' = Col.make "origin" Type.text origin
    let public' = Col.make "public" Type.bool public
    let table =
      let primary_key = Table.Primary_key.make [Def id'] in
      let foreign_keys =
        [ Table.Foreign_key.make
            ~cols:[Def reference']
            ~parent:(Table (reference_table, [Def reference_id']))
            ~on_delete:`Cascade (); ]
      in
      Table.make "reference_doc" ~primary_key ~foreign_keys @@
      Row.(unit row * id' * reference' * blob_key' * media_type' * name' *
           origin' * public')
  end

  include Doc
  include Entity.Publicable_queries (Doc)

  open Rel_query.Syntax

(*  let create d = Rel_sql.insert_into Db.dialect table d *)
  let of_ref_ids ~only_public rids =
    let* rid = rids in
    let* doc = Bag.table table in
    let filter = Bool.(not only_public || doc #. public') in
    Bag.where (Reference.Id.(rid = doc #. reference') && filter) (Bag.yield doc)
end

include Entity.Publicable_queries (Reference)

open Rel_query.Syntax

let ids_of_refs refs = let* r = refs in Bag.yield (r #. id')

let containers_of_refs ~only_public refs =
  let* c = Bag.table Container.table in
  let is_used =
    Bag.exists @@
    let* ref = refs in
    let eq_container =
      Option.has_value ~eq:Container.Id.( = ) (c #. Container.id')
    in
    let is_used = eq_container (ref #. container') in
    let filter = Bool.(not only_public || c #. Container.public') in
    Bag.where Bool.(is_used && filter) (Bag.yield c)
  in
  Bag.where is_used (Bag.yield c)

let filter_person_id pid refs =
  let* r = refs in
  let rid = r #. id' in
  let* c = Bag.table Contributor.table in
  let is_used =
    Bag.exists @@
    Bag.where
      (Person.Id.(c #. Contributor.person' = pid) &&
       Id.(c #. Contributor.reference' = rid))
      (Bag.yield Bool.true')
  in
  Bag.where is_used (Bag.yield r)

let filter_container_id cid refs =
  let* r = refs in
  let has_cid =
    Option.(has_value ~eq:Container.Id.( = ) cid (r #. container'))
  in
  Bag.where has_cid (Bag.yield r)

let author_ids_stmt =
  (* FIXME query language support for coded column!
let authors refs =
  let* r = refs in
  let* c = Bag.table Contributor.table in
  let of_ref = Int.(c #. Contributor.reference' = r #. id') in
  let is_author = c #. Contributor.role' = Bag.inj Person.Author in
  Bag.where (of_ref && is_author) (Bag.yield ( c #. Contributor.person')) *)
  let sql =
    "SELECT c.person FROM reference as r, reference_contributor as c  \
     WHERE r.id = ?1 AND r.id = c.reference AND c.role = 0"
  in
  Rel_sql.Stmt.(func sql @@
                Id.type' @-> ret (Row.(t1 (col "id" Person.Id.type'))))

(* FIXME were is my nice query language ? *)
(* FIXME also if we move to the query language we can't make a single
   query to list all and return their ref count, we'd need left join
   support. *)

let person_ref_count_row = Row.(t2 (col "id" Person.Id.type') (int "ref_count"))
let persons_public_ref_count_stmt =
  let sql =
    "SELECT c.person, COUNT(*)
     FROM
      reference as r,
      reference_contributor as c
     WHERE r.id = c.reference AND r.public
     GROUP BY c.person"
  in
  Rel_sql.Stmt.(func sql @@ ret person_ref_count_row)

let person_ref_count_stmt =
  (* FIXME rel aggregations. FIXME this is wrong if multiple contrib *)
  let sql =
    "SELECT COUNT(*)
     FROM reference_contributor as c
     WHERE c.person = ?1"
  in
  Rel_sql.Stmt.(func sql @@ Person.Id.type' @-> ret Row.(t1 @@ int "ref_count"))

let container_ref_count_row =
  Row.(t2 (col "id" Container.Id.type') (int "ref_count"))

let container_public_ref_count_stmt =
  let sql =
    "SELECT r.container, COUNT(*)
     FROM reference as r
     WHERE r.public AND r.container IS NOT NULL
     GROUP BY r.container"
  in
  Rel_sql.Stmt.(func sql @@ ret container_ref_count_row)

let container_ref_count_stmt =
  let sql =
    "SELECT COUNT(*)
     FROM reference as r
     WHERE r.container = ?1"
  in
  Rel_sql.Stmt.(func sql @@
                Container.Id.type' @-> ret Row.(t1 @@ int "ref_count"))

let subject_ref_count_row =
  Row.(t2 (col "id" subject_id_type) (int "ref_count"))

let subject_public_ref_count_stmt =
  let sql =
    "SELECT s.subject, COUNT(*)
     FROM reference as r, reference_subject as s
     WHERE r.public AND r.id = s.reference
     GROUP BY s.subject"
  in
  Rel_sql.Stmt.(func sql @@ ret subject_ref_count_row)

let replace_container_stmt ~this ~by =
  let this = Col.Value (container', (Some this)) in
  let by = Col.Value (container', (Some by)) in
  Rel_sql.update Db.dialect table ~set:[by] ~where:[this]

open Schema_kit

let ids_citing_doi doi =
  let* c = Bag.table Cites.table in
  Bag.where Doi_rel.(c #. Cites.doi' = doi) (Bag.yield (c #. Cites.reference'))

let citing_doi doi =
  let* citing = ids_citing_doi doi in
  let* r = Bag.table table in
  Bag.where Id.(r #. id' = citing) (Bag.yield r)

let dois_cited rid =
  let* c = Bag.table Cites.table in
  Bag.where Id.(c #. Cites.reference' = rid) (Bag.yield (c #. Cites.doi'))

let find_dois dois =
  let* doi = dois in
  let* r = Bag.table table in
  let eq_doi =
    Option.(equal (r #.doi') (some Doi_rel.t doi) ~eq:Doi_rel.equal)
  in
  Bag.where eq_doi (Bag.yield r)

let find_doi doi =
  let* r = Bag.table table in
  let eq_doi =
    Option.(equal (r #. doi') (some Doi_rel.t doi) ~eq:Doi_rel.equal)
  in
  Bag.where eq_doi (Bag.yield r)

let id_map db st id =
  let add r acc = Id.Map.add (id r) r acc in
  Db.fold db st add Id.Map.empty

let id_map_list db st id =
  let add r acc = Id.Map.add_to_list (id r) r acc in
  Db.fold db st add Id.Map.empty


let id_map_related_list :
  ?order:('b -> 'b -> int) ->
  Db.t -> 'a Rel_sql.Stmt.t -> id:('a -> Id.t) -> related:('a -> 'id) ->
  related_by_id:('id -> 'b option) -> ('b list Id.Map.t, Db.error) result
  =
  fun ?order db rel_stmt ~id ~related ~related_by_id ->
  let add r acc = match related_by_id (related r) with
  | None (* if the read is not in a transaction *) -> acc
  | Some p -> Id.Map.add_to_list (id r) p acc
  in
  let open Result.Syntax in
  let* m = Db.fold db rel_stmt add Id.Map.empty in
  match order with
  | None -> Ok m
  | Some order -> Ok (Id.Map.map (List.sort order) m)

type render_data =
  { list : t list;
    labels : label list Id.Map.t;
    authors : Person.t list Id.Map.t;
    containers : Container.t Container.Id.Map.t; (* mapped by container id *)
    editors : Person.t list Id.Map.t;
    subjects : subject list Id.Map.t;
    docs : Doc.t list Id.Map.t }

let render_data ~only_public refs =
  let ref_ids = ids_of_refs refs in
  let ref_labels = Label.applications ref_ids in
  let ref_contributors = Contributor.of_ref_ids ref_ids in
  let ref_subjects = Subject.of_ref_ids ref_ids in
  let docs = Doc.of_ref_ids ~only_public ref_ids in
  let labels = Label.of_applications ~only_public ref_labels in
  let persons = Contributor.persons ~only_public ref_contributors in
  let subjects = Subject.subjects ~only_public ref_subjects in
  let containers = containers_of_refs ~only_public refs in
  let ref_labels_stmt = Rel_query.Sql.of_bag' Label.table ref_labels in
  let ref_contributors_stmt =
    Rel_query.Sql.of_bag' Contributor.table ref_contributors in
  let ref_subjects_stmt = Rel_query.Sql.of_bag' Subject.table ref_subjects in
  let docs_stmt = Rel_query.Sql.of_bag' Doc.table docs in
  let labels_stmt = Rel_query.Sql.of_bag' label_table labels in
  let persons_stmt = Rel_query.Sql.of_bag' Person.table persons  in
  let subjects_stmt = Rel_query.Sql.of_bag' subject_table subjects in
  let refs_stmt = Rel_query.Sql.of_bag' table refs in
  let containers_stmt = Rel_query.Sql.of_bag' Container.table containers in
  fun db ->
    let open Result.Syntax in
    let* list = Db.list db refs_stmt in
    let* ps = Person.id_map db persons_stmt Person.id in
    let* ss = subject_id_map db subjects_stmt subject_id in
    let* ls = label_id_map db labels_stmt label_id in
    let* docs = id_map_list db docs_stmt Doc.reference in
    let* labels =
      let id = Label.entity and related = Label.label in
      let related_by_id l = label_id_map_find_opt l ls in
      let order = label_order_by_name in
      id_map_related_list
        db ref_labels_stmt ~order ~id ~related ~related_by_id
    in
    (* Once we have sort by support with rel we can directly
       fold on ref_contributors_stmts *)
    let* contributors = Db.list db ref_contributors_stmt in
    let contributors =
      let by_rev_position = Fun.flip Contributor.order_by_position in
      List.sort by_rev_position contributors
    in
    let authors, editors =
      let add (a, e) c =
        match Person.Id.Map.find_opt (Contributor.person c) ps with
        | None -> (a, e)
        | Some p ->
            match Contributor.role c with
            | Author -> Id.Map.add_to_list (Contributor.reference c) p a, e
            | Editor -> a, Id.Map.add_to_list (Contributor.reference c) p e
      in
      List.fold_left add (Id.Map.empty, Id.Map.empty) contributors
    in
    let* subjects =
      let id = Subject.reference and related = Subject.subject in
      let related_by_id s = subject_id_map_find_opt s ss in
      let order = subject_order_by_name in
      id_map_related_list
        db ref_subjects_stmt ~order ~id ~related ~related_by_id
    in
    let* containers = Container.id_map db containers_stmt Container.id in
    Ok { list; labels; authors; containers; editors; subjects; docs }

module Url = struct
  open Result.Syntax

  type named_id = string option * Id.t
  type t =
  | Change_authors_publicity of Id.t
  | Confirm_delete of Id.t
  | Create
  | Delete of Id.t
  | Doc of named_id * Doc.Id.t
(*
  | Duplicate of Id.t
  | Duplicate_form of Id.t
*)
  | Edit_form of Id.t
  | Fill_in_form of [`Doi of string ]
  | Index
  | New_form of { cancel : Entity.Url.cancel_url }
  | Page of named_id
(*  | Replace of Id.t
    | Replace_form of Id.t *)
  | Update of Id.t
  | View_fields of Id.t

  let doi = "doi"
  let get_doi u = match Http.Query.find_first doi (Kurl.Bare.query u) with
  | None -> Http.Response.bad_request_400 ()
  | Some doi -> Ok doi

  let dec u = match Kurl.Bare.path u with
  | [""] ->
      let* meth = Kurl.allow Http.Method.[get; post] u in
      let url = match meth with `GET -> Index | `POST -> Create in
      Kurl.ok url
  | ["part"; "confirm-delete"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Confirm_delete id)
  | ["part"; "edit-form"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Edit_form id)
  | ["part"; "fill-in-form"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let* doi = get_doi u in
      Kurl.ok (Fill_in_form (`Doi doi))
  | ["part"; "view-fields"; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (View_fields id)
(*
  | ["part"; "replace-form"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Replace_form id)
  | ["part"; "duplicate-form"; id] ->
      let* `GET, id = Entity.Url.get_id u id in
      Kurl.ok (Duplicate_form id)
*)
  | ["part"; "new-form"] ->
      let* `GET = Kurl.allow Http.Method.[get] u in
      let cancel = Entity.Url.cancel_url_of_query (Kurl.Bare.query u) in
      Kurl.ok (New_form { cancel })
(*
  | ["action"; "duplicate"; id] ->
      let* `POST, id = Entity.Url.meth_id u Kurl.Allow.[post] id in
      Kurl.ok (Duplicate id)
  | ["action"; "replace"; id] ->
      let* `POST, id = Entity.Url.meth_id u Kurl.Allow.[post] id in
      Kurl.ok (Replace id)
*)
  | ["action"; "change-authors-publicity"; id] ->
      let* `POST, id = Entity.Url.meth_id (module Id) u Http.Method.[post] id in
      Kurl.ok (Change_authors_publicity id)
  | [name; id; "docs"; docid] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      (* FIXME don't we have something to easily error here ? *)
      let error_to_resp e =
        Http.Response.empty ~log:e Http.Status.bad_request_400
      in
      let* docid = Doc.Id.of_string docid |> Result.map_error error_to_resp in
      Kurl.ok (Doc ((Some name, id), docid))
  | [name; id] ->
      let* `GET, id = Entity.Url.get_id (module Id) u id in
      Kurl.ok (Page (Some name, id))
  | [id] ->
      let* meth, id =
        Entity.Url.meth_id (module Id) u Http.Method.[get; put; delete] id
      in
      let url = match meth with
      | `GET -> Page (None, id) | `PUT -> Update id | `DELETE -> Delete id
      in
      Kurl.ok url
  | _ ->
      Kurl.no_match

  let html = ".html"
  let enc = function
  | Change_authors_publicity id ->
      Kurl.bare `POST
        ["action"; "change-authors-publicity"; Id.to_string id]
  | Confirm_delete id ->
      Kurl.bare `GET ["part"; "confirm-delete"; Id.to_string id]
  | Create ->
      Kurl.bare `POST [""]
  | Delete id ->
      Kurl.bare `DELETE [Id.to_string id]
  | Doc ((Some n, id), docid) ->
      Kurl.bare `GET [n; Id.to_string id; "docs"; Doc.Id.to_string docid]
  | Doc ((None, id), docid) ->
      Kurl.bare `GET [Id.to_string id; "docs"; Doc.Id.to_string docid]
(*
  | Duplicate id ->
      Kurl.bare `POST ["action"; "duplicate"; Id.to_string id]
  | Duplicate_form id ->
      Kurl.bare `GET ["part"; "duplicate-form"; Id.to_string id]
*)
  | Edit_form id ->
      Kurl.bare `GET ["part"; "edit-form"; Id.to_string id]
  | Index ->
      Kurl.bare `GET [""] ~ext:html
  | Fill_in_form (`Doi d) ->
      (* XXX something feels wrong with Kurl here separate
         URL req / resp types ? *)
      let query = match d with
      | "" -> None | d -> Some (Http.Query.empty |> Http.Query.def doi d)
      in
      Kurl.bare `GET ["part"; "fill-in-form"] ?query
  | New_form { cancel } ->
      let query = Entity.Url.cancel_url_to_query cancel in
      Kurl.bare `GET ["part"; "new-form"] ?query
  | Page (None, id) ->
      Kurl.bare `GET [Id.to_string id] ~ext:html
  | Page (Some n, id) ->
      Kurl.bare `GET [n; Id.to_string id] ~ext:html
(*
  | Replace id ->
      Kurl.bare `POST ["action"; "replace"; Id.to_string id]
  | Replace_form id ->
      Kurl.bare `GET ["part"; "replace-form"; Id.to_string id]
*)
  | Update id ->
      Kurl.bare `PUT [Id.to_string id]
  | View_fields id  ->
      Kurl.bare `GET ["part"; "view-fields"; Id.to_string id]

  let kind = Kurl.kind ~name:"reference" enc dec
  let v u = Kurl.v kind u

  (* Constructors *)

  let res_name r = Res.Named.name_of_string (title r)
  let page r = Kurl.v kind (Page (Some (res_name r), id r))

  let doc r doc =
    let named_id = Some (res_name r), id r and docid = Doc.id doc in
    Kurl.v kind (Doc (named_id, docid))
end
