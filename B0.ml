open B0_kit.V000
open Result.Syntax

(* OCaml libraries *)

let b0_std = B0_ocaml.libname "b0.std"
let brr = B0_ocaml.libname "brr"
let bytesrw = B0_ocaml.libname "bytesrw"
let bytesrw_xxhash = B0_ocaml.libname "bytesrw.xxhash"
let bytesrw_unix = B0_ocaml.libname "bytesrw.unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let htmlact = B0_ocaml.libname "htmlact"
let htmlact_page = B0_ocaml.libname "htmlact.page"
let htmlit = B0_ocaml.libname "htmlit"
let jsont_bytesrw = B0_ocaml.libname "jsont.bytesrw"
let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"
let ptime_clock = B0_ocaml.libname "ptime.clock"
let rel = B0_ocaml.libname "rel"
let rel_cli = B0_ocaml.libname "rel.cli"
let rel_pool = B0_ocaml.libname "rel.pool"
let rel_sqlite3 = B0_ocaml.libname "rel.sqlite3"
let threads = B0_ocaml.libname "threads"
let typegist = B0_ocaml.libname "typegist"
let unix = B0_ocaml.libname "unix"
let webs = B0_ocaml.libname "webs"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_unix = B0_ocaml.libname "webs.unix"

let hyperbib_base = B0_ocaml.libname "hyperbib.base"

(* Front end (and copy to app/static) *)

let static_dir = ~/"app/static"
let src_front_dir = ~/"src/front"

let assets_to_static_dir b =
  let open Fut.Syntax in
  let m = B0_build.memo b in
  B0_memo.run_proc m @@ fun () ->
  let dir = B0_build.in_scope_dir b src_front_dir in
  let dst = B0_build.in_scope_dir b static_dir in
  let* assets = B0_srcs.select b [`Dir_rec dir] in
  let assets = B0_srcs.by_ext assets in
  let exts = B0_file_exts.www in
  let _ = B0_jsoo.copy_assets m assets ~exts ~assets_root:(Some dir) ~dst in
  Fut.return ()

let front_to_static_dir b =
  let open Fut.Syntax in
  let m = B0_build.memo b in
  B0_memo.run_proc m @@ fun () ->
  let dst = B0_build.in_scope_dir b static_dir in
  let exe = B0_unit.get_meta B0_unit.exe_file (B0_build.current b) in
  let* exe = B0_memo.fail_if_error m exe in
  B0_memo.copy m exe ~dst:Fpath.(dst / basename exe);
  let has_map = B0_unit.find_meta B0_jsoo.source_map (B0_build.current b) in
  let has_map = match Option.join has_map with
  | Some `File -> true | Some `Inline | None -> false
  in
  if has_map then begin
    let map = Fpath.(exe -+ ".map") in
    B0_memo.copy m map ~dst:Fpath.(dst / basename map);
  end;
  Fut.return ()

let hyperbib_js =
  let srcs = [ `Dir src_front_dir ] in
  let doc = "hyperbib front-end" in
  let requires = [brr; note; note_brr; htmlact_page] in
  let wrap proc b = assets_to_static_dir b; front_to_static_dir b; proc b in
  B0_jsoo.exe "hyperbib.js" ~requires ~wrap ~doc ~srcs

(* Backend *)

let static_files = ["hyperbib.css"; "hyperbib.js" ]
let stamp_ml b = B0_build.in_current_dir b ~/"stamp.ml"
let stamp_mli b = B0_build.in_current_dir b ~/"stamp.mli"
let stamp_mli_src = ~/"src/stamp.mli"

let vcs_describe b =
  (* TODO b0: that wouldn't work in a release. Implement B0 watermarks. *)
  (* TODO b0: memo ? *)
  let open Result.Syntax in
  let dir = B0_build.scope_dir b in
  let* vcs = B0_vcs_repo.get () ~dir in
  B0_vcs_repo.describe vcs ~dirty_mark:true "HEAD"

let write_static_file_stamp b =
  let m = B0_build.memo b in
  let r = B0_memo.reviver m in
  let asset_file file = B0_build.in_scope_dir b Fpath.(static_dir / file) in
  let files = List.map asset_file static_files in
  let stamp_ml = stamp_ml b in
  let version = vcs_describe b |> B0_memo.fail_if_error m in
  B0_memo.write m ~stamp:version ~reads:files stamp_ml @@ begin fun () ->
    let stamp_file f =
      B0_zero.Reviver.hash_file r f |> B0_memo.fail_if_error m
    in
    let stamps =
      List.map (fun f -> Hash.to_binary_string (stamp_file f)) files
    in
    let stamp = B0_zero.Reviver.hash_string r (String.concat "" stamps) in
    Ok (Fmt.str "let static_files = %S\nlet version = %S" (Hash.to_hex stamp)
          version)
  end;
  let mli = B0_build.in_scope_dir b stamp_mli_src in
  B0_memo.ready_file m mli;
  B0_memo.copy m mli ~dst:(stamp_mli b)

let hyperbib_base_requires =
  [ unix; threads; cmdliner; bytesrw; bytesrw_xxhash; bytesrw_unix;
    ptime_clock; b0_std; jsont_bytesrw; htmlit; htmlact;
    rel; rel_cli; rel_sqlite3; rel_pool; typegist;
    webs; webs_kit; webs_unix; webs_cli; ]

let hyperbib_requires =
  hyperbib_base :: hyperbib_base_requires (* remove once we have -H support *)

let hyperbib_base_lib =
  let doc = "hyperbib components" in
  let srcs =
    let stamp b =
      (* TODO b0: slightly messy we need to copy it over because of
         https://github.com/ocaml/ocaml/issues/9717
         Maybe we should rather always copy srcs to build_dir *)
      Fut.return (Fpath.Set.(singleton (stamp_ml b) |> add (stamp_mli b)))
    in
    [ `Dir ~/"src";
      `Dir ~/"src/service";
      `Dir ~/"src/schema";
      `Dir ~/"src/html";
      `Dir ~/"src/moveout";
      `X stamp_mli_src;
      `Fut stamp ]
  in
  let requires = hyperbib_base_requires in
  let exports = hyperbib_base_requires in
  let wrap proc b = write_static_file_stamp b; proc b in
  B0_ocaml.lib hyperbib_base ~doc ~srcs ~requires ~exports ~wrap

let hyperbib =
  let doc = "hyperbib tool" in
  let srcs = [ `Dir ~/"src/tool" ] in
  let meta =
    B0_meta.empty
    (* TODO b0: supported_code doesn't work. *)
    |> ~~ B0_ocaml.Code.needs `Native
    |> ~~ B0_unit.Action.cwd (`In (`Scope_dir, ~/"app"))
  in
  let wrap proc b = B0_build.require_unit b hyperbib_js; proc b in
  let requires = hyperbib_requires in
  B0_ocaml.exe "hyperbib" ~public:true ~wrap ~doc ~srcs ~requires ~meta

(* Test *)

let test ?doc ?run:(r = true) ?(requires = []) ?(srcs = []) src =
  let srcs = (`File src) :: srcs in
  let requires = hyperbib_base :: requires in
  let meta = B0_meta.(empty |> tag test |> ~~ run r) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ?doc

let test_blobstore = test ~/"test/test_blobstore.ml"
let test_orcid = test ~/"test/test_orcid.ml"

(* Actions *)

let deploy_remote = "philo:"
let pull_data =
  B0_unit.of_action "pull-data" ~doc:"Pull live data into dev database" @@
  fun env _ ~args ->
  let* rsync = B0_rsync.get () in
  let src = ~/"hyperbib/app/data/bib.sqlite3.backup" in
  let dst = B0_env.in_scope_dir env ~/"app/data/bib.sqlite3" in
  B0_rsync.copy rsync ~delete:true ~src_host:deploy_remote src ~dst

let exec_remote cmd = Cmd.(arg "ssh" % "-t" % "philo" % cmd)

let logs_cmd name = Fmt.str "sudo journalctl -a -f -u %s" name
let deploy_cmd name =
  Fmt.str "cd %s && eval $(opam env) && b0 && \
           sudo systemctl restart %s && \
           sudo systemctl status %s" name name name

let deploy_test =
  B0_unit.of_action "deploy-test" ~doc:"Build and deploy on test server" @@
  fun _ _ ~args -> Os.Cmd.run (exec_remote (deploy_cmd "hyperbib-next"))

let deploy_live =
  B0_unit.of_action "deploy-live" ~doc:"Build and deploy on the live server" @@
  fun _ _ ~args -> Os.Cmd.run (exec_remote (deploy_cmd "hyperbib"))

let test_logs =
  B0_unit.of_action "test-logs" ~doc:"Test server logs" @@
  fun _ _ ~args -> Os.Cmd.run (exec_remote (logs_cmd "hyperbib-next"))

let live_logs =
  B0_unit.of_action "live-logs" ~doc:"Live server logs" @@
  fun _ _ ~args -> Os.Cmd.run (exec_remote (logs_cmd "hyperbib"))

let schema =
  B0_unit.of_action "schema" ~doc:"Show schema" ~units:[hyperbib] @@
  fun env _ ~args:_ ->
  let open Result.Syntax in
  (* b0 FIXME: we should readd pipes to Os.Cmd… *)
  let* hyperbib = B0_env.unit_exe_file_cmd env hyperbib in
  let* dot = B0_env.get_cmd env (Cmd.tool "dot") in
  let* show_url = B0_env.get_cmd env (Cmd.tool "show-url") in
  let* graph =
    let cwd =  B0_env.in_scope_dir env ~/"app" in
    Os.Cmd.run_out ~cwd ~trim:false
      Cmd.(hyperbib % "db" % "schema" % "-fdot" % "app")
  in
  let stdin = Os.Cmd.in_string graph in
  let* svg = Os.Cmd.run_out ~stdin ~trim:false Cmd.(dot % "-Tsvg") in
  let stdin = Os.Cmd.in_string svg in
  Os.Cmd.run ~stdin Cmd.(show_url % "-t" % "schema.svg")






(* Packs *)

let all = B0_pack.make "all" ~locked:true @@ B0_unit.list ()

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The hyperbib programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/hyperbib"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/hyperbib/doc"
    |> ~~ B0_meta.licenses ["ISC"; "OFL-1.1"; "MIT"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/hyperbib.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/hyperbib/issues"
    |> ~~ B0_meta.description_tags ["app"; "bibliography"; "org:erratique"]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "5.2.0"|};
        "ocamlfind", {|build|};
        "b0", {|build|};
        "cmdliner", {|>= "1.3.0"|};
        "bytesrw", {||};
        "ptime", {|>= "1.2.0"|};
        "webs", {||};
        "htmlit", {||};
        "htmlact", {||};
        "conf-sqlite2", {||};
        "conf-xxhash", {||};
        "conf-zstd", {||};
        "rel", {||};
        "note", {||};
        "brr", {||};
        "js_of_ocaml", {||};]
    |> ~~ B0_opam.pin_depends
      [ "bytesrw.dev", "git+https://erratique.ch/repos/bytesrw.git#master";
        "jsont.dev", "git+https://erratique.ch/repos/jsont.git#master";
        "htmlact.dev", "git+https://erratique.ch/repos/htmlact.git#master";
        "rel.dev", "git+https://erratique.ch/repos/rel.git#master";
        "typegist.dev", "git+https://erratique.ch/repos/typegist.git#master";
        "webs.dev", "git+https://erratique.ch/repos/webs.git#master"]
    |> ~~ B0_opam.build {|[["b0"]]|}
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~meta ~locked:true @@
  B0_unit.list ()
