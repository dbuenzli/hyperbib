(*---------------------------------------------------------------------------
   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Typegist

let bytes_as_utf_8_jsont ~kind ?doc () =
  let dec = Bytes.of_string and enc = Bytes.to_string in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.string

let bytes_as_bytes_jsont ~kind ?doc () =
  let dec = Bytes.of_string and enc = Bytes.to_string in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.binary_string

let nativeint_jsont ~kind ?doc () =
  let dec = Int64.to_nativeint and enc = Int64.of_nativeint in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.int64

let char_jsont ~kind ?doc () =
  let err_exp ~fnd = Jsont.Error.expected Jsont.Meta.none "one hex byte" ~fnd in
  let dec = function
  | "" -> err_exp ~fnd:{|""|}
  | s when String.length s > 1 -> err_exp ~fnd:"multiple bytes"
  | s -> s.[0]
  in
  let enc c = String.make 1 c in
  Jsont.map ~kind ?doc ~dec ~enc Jsont.binary_string

let uchar_jsont ~kind ?doc () =
  let err_exp meta ~fnd =
    Jsont.Error.expected meta "one Unicode character" ~fnd
  in
  let dec meta = function
  | "" -> err_exp meta ~fnd:{|""|}
  | s ->
      (* N.B. Jsont guarantees UTF-8 validity on input. *)
      let dec = String.get_utf_8_uchar s 0 in
      let len = Uchar.utf_decode_length dec in
      if len <> String.length s
      then err_exp meta ~fnd:"multiple characters"
      else Uchar.utf_decode_uchar dec
  in
  let enc u =
    let b = Bytes.create (Uchar.utf_8_byte_length u) in
    ignore (Bytes.set_utf_8_uchar b 0 u);
    Bytes.unsafe_to_string b
  in
  Jsont.Base.string (Jsont.Base.map ~kind ?doc ~dec ~enc ())

(* Deriving *)

module Meta = struct
  module Mem_map = Type.Gist.Meta.Key
      (struct type ('o, 'a) t = ('o, 'a) Jsont.Object.Mem.map end)

  module Mem_name = Type.Gist.Meta.Key
      (struct type ('o, 'a) t = string end)

  module Mem_enc_omit = Type.Gist.Meta.Key
      (struct type ('o, 'a) t = 'a -> bool end)

  module Jsont = Type.Gist.Meta.Key (struct type ('a, 'b) t = 'a Jsont.t end)
end

let maplike_binding_jsont key value  =
  Jsont.Object.map (fun k v -> k, v)
  |> Jsont.Object.mem "key" key ~enc:fst
  |> Jsont.Object.mem "value" value ~enc:snd
  |> Jsont.Object.finish

let rec scalar_jsont : type a. a Type.Gist.scalar -> a Jsont.t =
fun s ->
  let meta = Type.Gist.Scalar.meta s in
  match Meta.Jsont.find meta with
  | Some jsont -> jsont
  | None ->
      let doc = Type.Gist.Meta.Doc.find meta in
      let kind = Type.Gist.Scalar.type_name s in
      match s with
      | Unit _ -> Jsont.null ~kind ?doc ()
      | Bool _ -> Jsont.with_doc ~kind ?doc Jsont.bool
      | Char _ -> char_jsont ~kind ?doc ()
      | Uchar _ -> uchar_jsont ~kind ?doc ()
      | Int _ -> Jsont.with_doc ~kind ?doc Jsont.int
      | Int32 _ -> Jsont.with_doc ~kind ?doc Jsont.int32
      | Int64 _ -> Jsont.with_doc ~kind ?doc Jsont.int64
      | Nativeint _ -> nativeint_jsont ~kind ?doc ()
      | Float _ -> Jsont.with_doc ~kind ?doc Jsont.number

and arraylike_jsont :
  type elt arr. (elt, arr) Type.Gist.arraylike -> arr Jsont.t
=
fun a ->
  let meta = Type.Gist.Arraylike.meta a in
  match Meta.Jsont.find meta with
  | Some jsont -> jsont
  | None ->
      let doc = Type.Gist.Meta.Doc.find meta in
      let kind = Type.Gist.Arraylike.type_name a in
      match a with
      | String (_, `Utf_8) -> Jsont.string
      | String (_, `Bytes) -> Jsont.binary_string
      | Bytes (_, `Bytes) -> bytes_as_bytes_jsont ~kind ?doc ()
      | Bytes (_, `Utf_8) -> bytes_as_utf_8_jsont ~kind ?doc ()
      | Array (_, elt) -> Jsont.array ~kind ?doc (to_jsont elt)
      | Bigarray1 (_, k, l, elt) ->
          Jsont.Array.(array @@ bigarray_map ~kind ?doc k l (to_jsont elt))
      | Array_module (_, (module A) , elt) ->
          (* XXX should we add an array builder to the ARRAY signature ? *)
          let dec a = A.init (Array.length a) (fun i -> a.(i)) in
          let enc a = Array.init (A.length a) (fun i -> A.get a i) in
          Jsont.map ~kind ?doc ~dec ~enc @@
          Jsont.array ~kind ?doc (to_jsont elt)

and maplike_jsont : type k v m. (k, v, m) Type.Gist.maplike -> m Jsont.t =
fun m -> match Meta.Jsont.find (Type.Gist.Maplike.meta m) with
| Some jsont -> jsont
| None ->
    let kind = Type.Gist.Maplike.type_name m in
    match m with
    | Map_module (meta, m, (Rec gkey), value) ->
        maplike_jsont (Type.Gist.Map_module (meta, m, Lazy.force gkey, value))
    | Hashtbl (meta, (Rec gkey), value) ->
        maplike_jsont (Type.Gist.Hashtbl (meta, Lazy.force gkey, value))
    | Map_module
        (meta, (module M), Type.Gist.Arraylike (String (kmeta, `Utf_8)), value)
      ->
        (* object as key-value map. TODO support more arraylikes it's
           a matter of converting to hex in the functionsb below. *)
        let doc = Type.Gist.Meta.Doc.find meta in
        let mems =
          let value = to_jsont value in
          let dec_empty () = M.empty in
          let dec_add _jmeta k v m = M.add k v m in
          let dec_finish _jmeta m = m in
          let enc f = M.fold (f Jsont.Meta.none) in
          Jsont.Object.Mems.map ~dec_empty ~dec_add ~dec_finish ~enc:{enc} value
        in
        Jsont.Object.map ?doc ~kind Fun.id
        |> Jsont.Object.keep_unknown mems ~enc:Fun.id
        |> Jsont.Object.finish
    | Map_module (meta, (module M), key, value) ->
        (* array of key/value objs *)
        let doc = Type.Gist.Meta.Doc.find meta in
        let dec_empty () = M.empty in
        let dec_add _ (k, v) m = M.add k v m in
        let dec_finish _jmeta _i m = m in
        let enc f acc m =
          let add k v (acc, i) = f acc i (k, v), i + 1 in
          fst (M.fold add m (acc, 0))
        in
        Jsont.Array.array @@
        Jsont.Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
          (maplike_binding_jsont (to_jsont key) (to_jsont value))
    | Hashtbl (meta, Type.Gist.Arraylike (String (kmeta, `Utf_8)), value) ->
        (* object as key-value map *)
        let doc = Type.Gist.Meta.Doc.find meta in
        let unknown_mems =
          let value = to_jsont value in
          let dec_empty () = Hashtbl.create ~random:true 10 in
          let dec_add _jmeta k v m = Hashtbl.add m k v; m in
          let dec_finish _jmeta h = h in
          let enc f = Hashtbl.fold (f Jsont.Meta.none) in
          Jsont.Object.keep_unknown ~enc:Fun.id @@
          Jsont.Object.Mems.map
            ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc} value
        in
        Jsont.Object.map ?doc ~kind Fun.id
        |> unknown_mems
        |> Jsont.Object.finish
    | Hashtbl (meta, key, value) ->
        (* array of key/value objs *)
        let doc = Type.Gist.Meta.Doc.find meta in
        let dec_empty () = Hashtbl.create ~random:true 10 in
        let dec_add _ (k, v) h = Hashtbl.add h k v; h in
        let dec_finish _jmeta _i h = h in
        let enc f acc m =
          let add k v (acc, i) = f acc i (k, v), i + 1 in
          fst (Hashtbl.fold add m (acc, 0))
        in
        Jsont.Array.array @@
        Jsont.Array.map ~kind ?doc ~dec_empty ~dec_add ~dec_finish ~enc:{enc}
          (maplike_binding_jsont (to_jsont key) (to_jsont value))

and product_jsont : type p. p Type.Gist.product -> p Jsont.t =
fun p ->
  let meta = Type.Gist.Product.meta p in
  match Meta.Jsont.find meta with
  | Some jsont -> jsont
  | None ->
      let doc = Type.Gist.Meta.Doc.find meta in
      let kind = Type.Gist.Product.name p in
      let field_mem n f =
        let meta = Type.Gist.Field.meta f in
        match Meta.Mem_map.find meta with
        | Some map -> map
        | None ->
            let doc = Type.Gist.Meta.Doc.find meta in
            let ignore =  match Type.Gist.Meta.Ignore.find meta with
            | Some false | None -> false | Some true -> true
            in
            let name = match Meta.Mem_name.find meta with
            | Some name -> name
            | None ->
                match Type.Gist.Field.name f with
                | "" -> Int.to_string n | name -> name
            in
            let enc = Type.Gist.Field.project f in
            let dec_absent = match Type.Gist.Field.default f with
            | None when ignore ->
                (* TODO better error message. *)
                invalid_arg "ignored field needs a default"
            | None | Some _ as v -> v
            in
            let value = to_jsont (Type.Gist.Field.gist f) in
            let enc_omit =
              if ignore then Some (fun _ -> true) else
              Meta.Mem_enc_omit.find meta
            in
            Jsont.Object.Mem.map ?doc ?dec_absent ?enc_omit ~enc name value
      in
      let rec loop :
        type o a. int -> (o, a) Type.Gist.fields -> (o, a) Jsont.Object.map
      =
      fun n fields -> match fields with
      | Type.Gist.Ctor f -> Jsont.Object.map ~kind ?doc f
      | Type.Gist.App (fs, f) ->
          Jsont.Object.Mem.app (loop (n + 1) fs) (field_mem n f)
      in
      let map = loop 0 (Type.Gist.Product.fields p) in
      Jsont.Object.finish map

and sum_jsont : type sum. sum Type.Gist.sum -> sum Jsont.t =
fun sum ->
  let meta = Type.Gist.Sum.meta sum in
  match Meta.Jsont.find meta with
  | Some jsont -> jsont
  | None ->
      let doc = Type.Gist.Meta.Doc.find meta in
      let kind = Type.Gist.Sum.type_name sum in
      match sum with
      | Option (_, t) -> Jsont.option ~kind ?doc (to_jsont t)
      | List (_, t) -> Jsont.list ~kind ?doc (to_jsont t)
      | sum ->
          let variant = Type.Gist.Sum.to_variant sum in
          let case_mem =
            let variant_case_jsont c =
              let tag = Type.Gist.Product.name c in
              Jsont.Object.Case.map ~dec:Fun.id tag (product_jsont c)
            in
            let cases = Type.Gist.Variant.cases variant in
            let cases = List.map variant_case_jsont cases in
            let cmap =
              let add_case m mc =
                let tag = Jsont.Object.Case.map_tag mc in
                Jsont.Repr.String_map.add tag mc m
              in
              List.fold_left add_case Jsont.Repr.String_map.empty cases
            in
            let enc_case value =
              let case = Type.Gist.Variant.project variant value in
              let tag = Type.Gist.Product.name case in
              let case = Option.get (Jsont.Repr.String_map.find_opt tag cmap) in
              Jsont.Object.Case.value case value
            in
            let cases = List.map Jsont.Object.Case.make cases in
            let tag_compare = String.compare and tag_to_string = Fun.id in
            let case_mem_name = match Meta.Mem_name.find meta with
            | None -> "case" | Some name -> name
            in
            Jsont.Object.case_mem case_mem_name Jsont.string ~tag_compare
              ~tag_to_string ~enc:Fun.id ~enc_case cases
          in
          Jsont.Object.map ?doc ~kind Fun.id |> case_mem |> Jsont.Object.finish

and func_jsont : type d r. (d, r) Type.Gist.func -> (d -> r) Jsont.t =
fun func -> match Meta.Jsont.find (Type.Gist.Func.meta func) with
| Some jsont -> jsont
| None ->
    let doc = Type.Gist.Meta.Doc.find (Type.Gist.Func.meta func) in
    let kind = "func" in
    let dec _ _ =
      (* We could have uids an a registry to rebind. *)
      fun _ -> invalid_arg "Could not recover serialized function"
    in
    let enc _ = Format.asprintf "%a" Type.Gist.pp_type (Type.Gist.Func func) in
    Jsont.Base.string (Jsont.Base.map ~kind ?doc ~dec ~enc ())

and abstract_jsont : type a. a Type.Gist.abstract -> a Jsont.t =
fun a -> match Meta.Jsont.find (Type.Gist.Abstract.meta a) with
| Some jsont -> jsont
| None ->
    let meta = Type.Gist.Abstract.meta a in
    let doc = Type.Gist.Meta.Doc.find meta in
    let kind = Type.Gist.Abstract.type_name a in
    let case_of_repr (Type.Gist.Abstract.Repr repr) =
      let repr_jsont =
        (* FIXME this may not be an object ! *)
        to_jsont (Type.Gist.Abstract.Repr.gist repr)
      in
      let tag = Type.Gist.Abstract.Repr.version repr in
      let dec = Type.Gist.Abstract.Repr.project repr in
      let case_map = Jsont.Object.Case.map ~dec tag repr_jsont in
      let enc_case v =
        Jsont.Object.Case.value case_map (Type.Gist.Abstract.Repr.inject repr v)
      in
      enc_case, Jsont.Object.Case.make case_map
    in
    let cases = List.map case_of_repr (Type.Gist.Abstract.reprs a) in
    let enc_case, _ = List.hd cases in
    let cases = List.map snd cases in
    let tag_compare = String.compare and tag_to_string = Fun.id in
    let case_mem_name = match Meta.Mem_name.find meta with
    | None -> "version" | Some name -> name
    in
    let dec_absent =
      (* The first representation maybe had no version *)
      Jsont.Object.Case.tag (List.hd (List.rev cases))
    in
    Jsont.Object.map ~kind ?doc Fun.id
    |> Jsont.Object.case_mem case_mem_name Jsont.string ~tag_compare
      ~dec_absent ~tag_to_string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.finish

and lazy_jsont : type a.
  kind:string -> a Lazy.t Type.Gist.Meta.t -> a Type.Gist.t -> a Lazy.t Jsont.t
=
fun ~kind meta g -> match Meta.Jsont.find meta with
| Some jsont -> jsont
| None ->
    let doc = Type.Gist.Meta.Doc.find meta in
    Jsont.map ~kind ?doc ~dec:Lazy.from_val ~enc:Lazy.force (to_jsont g)

and ref_jsont : type a.
  kind:string -> a ref Type.Gist.Meta.t -> a Type.Gist.t -> a ref Jsont.t
=
fun ~kind meta g -> match Meta.Jsont.find meta with
| Some jsont -> jsont
| None ->
    let doc = Type.Gist.Meta.Doc.find meta in
    Jsont.map ~kind ?doc ~dec:(fun v -> ref v) ~enc:( ! ) (to_jsont g)

and to_jsont : type a. a Type.Gist.t -> a Jsont.t =
fun g -> match g with
| Scalar s -> scalar_jsont s
| Arraylike a -> arraylike_jsont a
| Maplike m -> maplike_jsont m
| Product p -> product_jsont p
| Record r -> product_jsont r
| Sum s -> sum_jsont s
| Func f -> func_jsont f
| Abstract a -> abstract_jsont a
| Lazy (meta, lg) -> lazy_jsont ~kind:(Type.Gist.type_name g) meta lg
| Ref (meta, rg) -> ref_jsont ~kind:(Type.Gist.type_name g) meta rg
| Rec g -> to_jsont (Lazy.force g)

(* Jsont type gist *)

type e_repr = Erepr : 'a Jsont.Repr.t -> e_repr
type e_gist = Egist : 'a Type.Gist.t -> e_gist
type 'a e_map = Emap : ('b, 'a) Jsont.Repr.map -> 'a e_map
type 'a e_array =
| Earray : ('a, 'elt, 'builder) Jsont.Repr.array_map -> 'a e_array

let meta_gist = Type.Gist.todo ~type_name:"Jsont.Meta.t" ()

let base_map_gist agist bgist =
  let base_map kind doc dec enc enc_meta : ('a, 'b) Jsont.Repr.base_map =
    { Jsont.Repr.kind; doc; dec; enc; enc_meta }
  in
  let kind (m : ('a, 'b) Jsont.Repr.base_map) = m.Jsont.Repr.kind in
  let doc (m : ('a, 'b) Jsont.Repr.base_map) = m.Jsont.Repr.doc in
  let dec (m : ('a, 'b) Jsont.Repr.base_map) = m.Jsont.Repr.dec in
  let enc (m : ('a, 'b) Jsont.Repr.base_map) = m.Jsont.Repr.enc in
  let enc_meta (m : ('a, 'b) Jsont.Repr.base_map) = m.Jsont.Repr.enc_meta in
  Type.Gist.record "Jsont.Repr.base_map" base_map
  |> Type.Gist.field "kind" Type.Gist.string_as_utf_8 kind
  |> Type.Gist.field "doc" Type.Gist.string_as_utf_8 doc
  |> Type.Gist.field "dec" Type.Gist.(meta_gist @-> agist @-> bgist) dec
  |> Type.Gist.field "enc" Type.Gist.(bgist @-> agist) enc
  |> Type.Gist.field "ent_meta" Type.Gist.(bgist @-> meta_gist) enc_meta
  |> Type.Gist.finish_record

let rec object_map_gist ogist = Type.Gist.todo ()

and map_gist agist bgist =
  let map kind doc dom dec enc : ('a, 'b) Jsont.Repr.map =
    { Jsont.Repr.kind; doc; dom; dec; enc }
  in
  let kind (m : ('a, 'b) Jsont.Repr.map) = m.Jsont.Repr.kind in
  let doc (m : ('a, 'b) Jsont.Repr.map) = m.Jsont.Repr.doc in
  let dom (m : ('a, 'b) Jsont.Repr.map) = m.Jsont.Repr.dom in
  let dec (m : ('a, 'b) Jsont.Repr.map) = m.Jsont.Repr.dec in
  let enc (m : ('a, 'b) Jsont.Repr.map) = m.Jsont.Repr.enc in
  Type.Gist.record "Jsont.Repr.map" map
  |> Type.Gist.field "kind" Type.Gist.string_as_utf_8 kind
  |> Type.Gist.field "doc" Type.Gist.string_as_utf_8 doc
  |> Type.Gist.field "dom" (repr_gist agist) dom
  |> Type.Gist.field "dec" Type.Gist.(agist @-> bgist) dec
  |> Type.Gist.field "enc" Type.Gist.(bgist @-> agist) enc
  |> Type.Gist.finish_record

and e_map_gist agist = Type.Gist.todo ()
(*  Type.Gist.case "Jsont_type.gist.Emap" (fun m -> Emap m)
  |> Type.Gist.dim (map_gist agist bgist) (function Emap m -> m)
  |> Type.Gist.finish_map *)

and e_array_gist agist = Type.Gist.todo ()

and array_gist array_gist elt_gist builder_gist =
  let map kind doc elt dec_empty dec_skip dec_add dec_finish enc_meta :
    ('array, 'elt, 'builder) Jsont.Repr.array_map =
    let enc _ _ _ = failwith "TODO typegist " in
    { Jsont.Repr.kind; doc; elt; dec_empty; dec_skip; dec_add; dec_finish;
      enc; enc_meta }
  in
  Type.Gist.record "Jsont.Repr.array_map" map
  |> Type.Gist.field "kind" Type.Gist.string_as_utf_8
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.kind)
  |> Type.Gist.field "doc" Type.Gist.string_as_utf_8
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.doc)
  |> Type.Gist.field "elt" (repr_gist elt_gist)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.elt)
  |> Type.Gist.field "dec_empty" Type.Gist.(unit @-> builder_gist)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.dec_empty)
  |> Type.Gist.field "dec_skip" Type.Gist.(int @-> builder_gist @-> bool)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.dec_skip)
  |> Type.Gist.field "dec_add"
     Type.Gist.(int @-> elt_gist @-> builder_gist @-> builder_gist)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.dec_add)
  |> Type.Gist.field "dec_finish"
     Type.Gist.(meta_gist @-> int @-> builder_gist @-> array_gist)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.dec_finish)
  |> Type.Gist.field "enc_meta" Type.Gist.(array_gist @-> meta_gist)
     (fun (m : ('a, 'e, 'b) Jsont.Repr.array_map) -> m.Jsont.Repr.enc_meta)
  |> Type.Gist.finish_record


and any_map_gist agist =
  let any_map kind doc dec_null dec_bool dec_number dec_string dec_array
      dec_object enc : 'a Jsont.Repr.any_map =
    { kind; doc; dec_null; dec_bool; dec_number; dec_string; dec_array;
      dec_object; enc }
  in
  let kind (m : 'a Jsont.Repr.any_map) = m.Jsont.Repr.kind in
  let doc (m : 'a Jsont.Repr.any_map) = m.Jsont.Repr.doc in
  let enc (m : 'a Jsont.Repr.any_map) = m.Jsont.Repr.enc in
  let a_repr = repr_gist agist in
  let a_repr_opt = Type.Gist.option a_repr in
  Type.Gist.record "Jsont.Repr.any_map" any_map
  |> Type.Gist.field "kind" Type.Gist.string_as_utf_8 kind
  |> Type.Gist.field "doc" Type.Gist.string_as_utf_8 doc
  |> Type.Gist.field "dec_null" a_repr_opt (fun m -> m.Jsont.Repr.dec_null)
  |> Type.Gist.field "dec_bool" a_repr_opt (fun m -> m.Jsont.Repr.dec_bool)
  |> Type.Gist.field "dec_number" a_repr_opt (fun m -> m.Jsont.Repr.dec_number)
  |> Type.Gist.field "dec_string" a_repr_opt (fun m -> m.Jsont.Repr.dec_string)
  |> Type.Gist.field "dec_array" a_repr_opt (fun m -> m.Jsont.Repr.dec_array)
  |> Type.Gist.field "dec_object" a_repr_opt (fun m -> m.Jsont.Repr.dec_object)
  |> Type.Gist.field "enc" Type.Gist.(agist @-> a_repr) enc
  |> Type.Gist.finish_record

and repr_gist agist =
  let rec g agist = lazy begin
    let g = Type.Gist.rec' (g agist) in
    let get_null = function Jsont.Repr.Null m -> m | _ -> assert false in
    let get_bool = function Jsont.Repr.Bool m -> m | _ -> assert false in
    let get_number = function Jsont.Repr.Number m -> m | _ -> assert false in
    let get_string = function Jsont.Repr.String m -> m | _ -> assert false in
    let get_array =
      function Jsont.Repr.Array m -> Earray m | _ -> assert false
    in
    let get_object = function Jsont.Repr.Object m -> m | _ -> assert false in
    let get_any = function Jsont.Repr.Any m -> m | _ -> assert false in
    let get_map = function Jsont.Repr.Map m -> Emap m | _ -> assert false in
    let get_rec = function Jsont.Repr.Rec l -> l | _ -> assert false in
    let null_case =
      Type.Gist.case "Jsont.Repr.Null" (fun m -> Jsont.Repr.Null m)
      |> Type.Gist.dim (base_map_gist Type.Gist.unit agist) get_null
      |> Type.Gist.finish_case
    in
    let bool_case =
      Type.Gist.case "Jsont.Repr.Bool" (fun m -> Jsont.Repr.Bool m)
      |> Type.Gist.dim (base_map_gist Type.Gist.bool agist) get_bool
      |> Type.Gist.finish_case
    in
    let number_case =
      Type.Gist.case "Jsont.Repr.Number" (fun m -> Jsont.Repr.Number m)
      |> Type.Gist.dim (base_map_gist Type.Gist.float agist) get_number
      |> Type.Gist.finish_case
    in
    let string_case =
      let base_map_gist = base_map_gist Type.Gist.string_as_utf_8 agist in
      Type.Gist.case "Jsont.Repr.String" (fun m -> Jsont.Repr.String m)
      |> Type.Gist.dim base_map_gist get_string
      |> Type.Gist.finish_case
    in
    let array_case =
      Type.Gist.case "Jsont.Repr.Array"
        (function Earray m -> Jsont.Repr.Array m)
      |> Type.Gist.dim (e_array_gist agist) get_array
      |> Type.Gist.finish_case
    in
    let object_case =
      Type.Gist.case "Jsont.Repr.Object" (fun m -> Jsont.Repr.Object m)
      |> Type.Gist.dim (object_map_gist agist) get_object
      |> Type.Gist.finish_case
    in
    let any_case =
      Type.Gist.case "Jsont.Repr.Any" (fun m -> Jsont.Repr.Any m)
      |> Type.Gist.dim (any_map_gist agist) get_any
      |> Type.Gist.finish_case
    in
    let map_case =
      Type.Gist.case "Jsont.Repr.Map" (function Emap m -> Jsont.Repr.Map m)
      |> Type.Gist.dim (e_map_gist agist) get_map
      |> Type.Gist.finish_case
    in
    let rec_case =
      Type.Gist.case "Jsont.Repr.Rec" (fun lt -> Jsont.Repr.Rec lt)
      |> Type.Gist.dim (Type.Gist.lazy' g) get_rec
      |> Type.Gist.finish_case
    in
    let get_case = function
    | Jsont.Repr.Null _ -> null_case
    | Jsont.Repr.Bool _ -> bool_case
    | Jsont.Repr.Number _ -> number_case
    | Jsont.Repr.String _ -> string_case
    | Jsont.Repr.Array _ -> array_case
    | Jsont.Repr.Object _ -> object_case
    | Jsont.Repr.Any _ -> any_case
    | Jsont.Repr.Map _ -> map_case
    | Jsont.Repr.Rec _ -> rec_case
    in
    let cases =
      [ null_case; bool_case; number_case; string_case; array_case;
        object_case; any_case; map_case; rec_case ]
    in
    Type.Gist.variant "Jsont.Repr.t" get_case cases
  end
  in
  Lazy.force (g agist)

let gist agist =
  let v1 =
    Type.Gist.Abstract.repr ~version:"1" (repr_gist agist)
      Jsont.Repr.of_t Jsont.Repr.unsafe_to_t
  in
  Type.Gist.abstract "Jsont.t" [ v1 ]
