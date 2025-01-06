(*---------------------------------------------------------------------------
   Copyright (c) 2025 The University of Bern. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_of_string () =
  Test.test "Orcid.of_string" @@ fun () ->
  let test ?__POS__ = Test.result ?__POS__ ~ok:(module Orcid) in
  let ok ?__POS__ id n = test (Orcid.of_string id) (Ok (Orcid.v n)) ?__POS__ in
  let error ?__POS__ id e = test (Orcid.of_string id) (Error e) ?__POS__ in
  ok "http://orcid.org/0000-0002-3843-3472" "0000-0002-3843-3472" ~__POS__;
  ok "https://orcid.org/0000-0001-7051-1197" "0000-0001-7051-1197" ~__POS__;
  ok "0000-0002-8205-121X" "0000-0002-8205-121X" ~__POS__;
  error "http://orcid.org/0000-0002-3843-347X"
    "ORCID checksum error, expected: '2' found: 'X'" ~__POS__;
  error "http://orcid.org/0000-0001-7051-1198"
    "ORCID checksum error, expected: '7' found: '8'" ~__POS__;
  error "https://orcid.org/0000-0002-8205-121A"
    "ORCID checksum error, expected: 'X' found: 'A'" ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  test_of_string ();
  ()

let () = if !Sys.interactive then () else exit (main ())
