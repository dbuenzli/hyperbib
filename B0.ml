open B0_kit.V000

(* OCaml libraries *)

let rel = B0_ocaml.libname "rel"
let rel_kit = B0_ocaml.libname "rel.kit"
let rel_cli = B0_ocaml.libname "rel.cli"
let rel_pool = B0_ocaml.libname "rel.pool"
let rel_sqlite3 = B0_ocaml.libname "rel.sqlite3"
let b0_std = B0_ocaml.libname "b0.std"
let b0 = B0_ocaml.libname "b0"
let b0_file = B0_ocaml.libname "b0.file"
let b0_kit = B0_ocaml.libname "b0.kit"
let brr = B0_ocaml.libname "brr"
let note = B0_ocaml.libname "note"
let note_brr = B0_ocaml.libname "note.brr"
let cmdliner = B0_ocaml.libname "cmdliner"
let htmlit = B0_ocaml.libname "htmlit"
let htmlact = B0_ocaml.libname "htmlact"
let htmlact_page = B0_ocaml.libname "htmlact.page"
let ptime = B0_ocaml.libname "ptime"
let ptime_clock = B0_ocaml.libname "ptime.clock.os"
let threads = B0_ocaml.libname "threads"
let webs = B0_ocaml.libname "webs"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_kit = B0_ocaml.libname "webs.kit"
let webs_unix = B0_ocaml.libname "webs.unix"

(* Front end (and copy to app/static) *)

let static_dir = Fpath.v "app/static"
let src_front_dir = Fpath.v "src/front"

let assets_to_static_dir b =
  let open Fut.Syntax in
  let m = B0_build.memo b in
  B0_memo.run_proc m @@ fun () ->
  let dir = B0_build.in_scope_dir b src_front_dir in
  let dst = B0_build.in_scope_dir b static_dir in
  let* assets = B0_srcs.select b Fpath.[ `Dir_rec dir ] in
  let assets = B0_srcs.by_ext assets in
  let exts = B0_file_exts.www in
  let _ = B0_jsoo.copy_assets m assets ~exts ~assets_root:(Some dir) ~dst in
  Fut.return ()

let front_to_static_dir b =
  let open Fut.Syntax in
  let m = B0_build.memo b in
  B0_memo.run_proc m @@ fun () ->
  let dst = B0_build.in_scope_dir b static_dir in
  let exe = B0_unit.get_meta B0_meta.exe_file (B0_build.current b) in
  let* exe = B0_memo.fail_if_error m exe in
  B0_memo.copy m ~src:exe Fpath.(dst / basename exe);
  let has_map = B0_unit.get_meta B0_jsoo.source_map (B0_build.current b) in
  let has_map = match B0_memo.fail_if_error m has_map with
  | Some `File -> true | Some `Inline | None -> false
  in
  if has_map then begin
    let map = Fpath.(exe -+ ".map") in
    B0_memo.copy m ~src:map Fpath.(dst / basename map);
  end;
  Fut.return ()

let hyperbib_js =
  let srcs = [ `Dir src_front_dir ] in
  let doc = "hyperbib front-end" in
  let requires = [brr; note; note_brr; htmlact_page] in
  let meta = B0_jsoo.meta ~requires () in
  let wrap proc b = assets_to_static_dir b; front_to_static_dir b; proc b in
  B0_jsoo.exe "hyperbib.js" ~wrap ~doc ~srcs ~meta

(* Backend *)

let static_files = ["hyperbib.css"; "hyperbib.js" ]
let stamp_ml b = B0_build.in_build_dir b Fpath.(v "stamp.ml")
let stamp_mli b = B0_build.in_build_dir b Fpath.(v "stamp.mli")
let stamp_mli_src = Fpath.v "src/stamp.mli"

let vcs_describe b =
  (* XXX that wouldn't work in a release. Implement B0 watermarks. *)
  (* XXX memo ? *)
  let open Result.Syntax in
  let dir = B0_build.scope_dir b (B0_build.current b) in
  let* vcs = B0_vcs.get () ~dir in
  B0_vcs.describe vcs ~dirty_mark:true "HEAD"

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
  B0_memo.file_ready m mli;
  B0_memo.copy m ~src:mli (stamp_mli b)

let hyperbib =
  let doc = "hyperbib tool" in
  let requires =
    [ threads; cmdliner; ptime; ptime_clock; b0_std; b0; b0_file; b0_kit;
      htmlit; htmlact;
      rel; rel_kit; rel_cli; rel_sqlite3; rel_pool;
      webs; webs_kit; webs_unix; webs_cli;
      ]
  in
  let srcs =
    (* XXX slightly messy we need to copy it over because of
       https://github.com/ocaml/ocaml/issues/9717
       Maybe we should rather always copy srcs to build_dir *)
    let stamp b =
      Fut.return (Fpath.Set.(singleton (stamp_ml b) |> add (stamp_mli b)))
    in
    Fpath.[`Dir (v "src");
           `Dir (v "src/service");
           `Dir (v "src/schema");
           `Dir (v "src/html");
           `X stamp_mli_src;
           `Fut stamp]
  in
  let meta =
    let app_dir b u = Fut.return Fpath.(B0_build.scope_dir b u / "app") in
    B0_meta.(empty
             (* B0 FIXME supported_code doesn't work. *)
             |> add B0_ocaml.Meta.needs_code `Native
             |> add B0_unit.Action.exec_cwd app_dir)
  in
  let wrap proc b =
    B0_build.require b hyperbib_js;
    write_static_file_stamp b;
    proc b
  in
  B0_ocaml.exe "hyperbib" ~wrap ~doc ~srcs ~requires ~meta

(* Cmdlets *)

let deploy_remote = "philo:"
let pull_data =
  let open Result.Syntax in
  B0_cmdlet.v "pull-data" ~doc:"Pull live data" @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  let* rsync = B0_rsync.get () in
  let src = Fpath.v "hyperbib/app/data/bib.sqlite3.backup" in
  let dst = B0_cmdlet.in_scope_dir env Fpath.(v "app/data/bib.sqlite3") in
  B0_rsync.copy rsync ~delete:true ~src_host:deploy_remote ~src dst

let exec_remote cmd = Cmd.(arg "ssh" % "-t" % "philo" % cmd)

let logs_cmd name = Fmt.str "sudo journalctl -a -f -u %s" name
let deploy_cmd name =
  Fmt.str "cd %s && eval $(opam env) && b0 && \
           sudo systemctl restart %s && \
           sudo systemctl status %s" name name name

let deploy_test =
  let open Result.Syntax in
  let doc = "Build and deploy on test server" in
  B0_cmdlet.v "deploy-test" ~doc @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  Os.Cmd.run (exec_remote (deploy_cmd "hyperbib-next"))

let deploy_live =
  let open Result.Syntax in
  let doc = "Build and deploy on the live server" in
  B0_cmdlet.v "deploy-live" ~doc @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  Os.Cmd.run (exec_remote (deploy_cmd "hyperbib"))

let test_logs =
  let open Result.Syntax in
  B0_cmdlet.v "test-logs" ~doc:"Test server logs" @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  Os.Cmd.run (exec_remote (logs_cmd "hyperbib-next"))

let live_logs =
  let open Result.Syntax in
  B0_cmdlet.v "live-logs" ~doc:"Live server logs" @@ fun env args ->
  B0_cmdlet.exit_of_result @@
  Os.Cmd.run (exec_remote (logs_cmd "hyperbib"))

(* Packs *)

let all = B0_pack.v "all" ~locked:true @@ B0_unit.list ()

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The hyperbib programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/hyperbib"
    |> add online_doc "https://erratique.ch/software/hyperbib/doc"
    |> add licenses ["ISC"; "OFL-1.1"; "MIT"]
    |> add repo "git+https://erratique.ch/repos/hyperbib.git"
    |> add issues "https://github.com/dbuenzli/hyperbib/issues"
    |> add description_tags ["app"; "bibliography"; "org:erratique"; ]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.12"|};
        "ocamlfind", {|build|};
        "b0", {|build|};
        "cmdliner", {|>= "1.0.4"|};
        "ptime", {||};
        "webs", {||};
        "htmlit", {||};
        "htmlact", {||};
        "rel", {||};
        "note", {||};
        "brr", {||};
        "js_of_ocaml", {||};]
    |> add B0_opam.Meta.pin_depends
      ["htmlact.~dev", "git+https://erratique.ch/repos/htmlact.git#master";
       "rel.~dev", "git+https://erratique.ch/repos/rel.git#master";
       "webs.~dev", "git+https://erratique.ch/repos/webs.git#master"]
    |> add B0_opam.Meta.build {|[["b0"]]|}
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~meta ~locked:true [hyperbib; hyperbib_js;]
