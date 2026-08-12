(** This file runs a battery of TLA+ syntax fragments against TLAPM's syntax
    parser. It then takes the resulting parse tree and translates it into a
    normalized S-expression form to compare with the associated expected AST.
    Tests are sourced from the standardized syntax test corpus at:
    https://github.com/tlaplus/rfcs/tree/2a772d9dd11acec5d7dedf30abfab91a49de48b8/language_standard/tests/tlaplus_syntax
*)

open Tlapm_lib;;

open Syntax_corpus_file_parser;;

let last_parse_error = ref "<none>"

(** Calls TLAPM's parser with the given input. Catches all exceptions and
    treats them as parse failures.
    @param input The TLA+ fragment to parse.
    @return None if parse failure, syntax tree root if successful.
*)
let parse (input : string) : Module.T.mule option =
  try module_of_string input
  with e ->
    last_parse_error := Printexc.to_string e;
    None

(** Names of tests that are known to succeed (but should fail) due to
    SANY bugs.
    @param test Information about the test.
    @return Whether the test is expected to fail.
*)
let sany_false_positive (test : syntax_test) : bool =
  List.mem test.info.name [
    "Label with Subexpression Prefix (GH tlaplus/tlaplus #885)";
    "Empty Tuple Quantification (GH tlaplus/tlaplus #888)";

    (* https://github.com/tlaplus/tlaplus/issues/616 *)
    "Invalid Use of LOCAL in LET/IN";
    "Invalid Use of LOCAL in Proof";
  ]

(** Names of tests that are expected to fail the tree comparison phase due to
    bugs in TLAPM's syntax parser.
    @param test Information about the test.
    @return Whether the test is expected to fail the tree comparison phase.
*)
let expect_tree_comparison_failure (test : syntax_test) : bool =
  List.mem test.info.name [
    (* TLAPM appears to simply return an empty set here? *)
    (* https://github.com/tlaplus/tlapm/issues/235 *)
    "Mistaken Set Filter Test";
    "Mistaken Set Filter Tuples Test";
  ]

open OUnit2;;

let run_test test _ =
  skip_if test.skip "Test has skip attribute";
  match test.test with
  | Error_test input -> (
    let b = sany_false_positive test in
    match parse input with
    | None -> assert_bool "Expected error test to fail" (not b)
    | Some _ -> assert_bool "Expected parse failure" b
  )
  | Expected_test (input, expected) -> (
      match parse input with
      | None ->
         let b = false in
         let msg = Printf.sprintf "Expected parse success, got: %S" !last_parse_error in
         assert_bool msg b
      | Some tlapm_output ->
        let open Tlapm_lib__Sany in
        let open Sexplib in
        let actual = module_to_sexp tlapm_output in
        let b = expect_tree_comparison_failure test in
        if Sexp.equal expected actual
        then assert_bool "Expected parse test to fail" (not b)
        else
          let open Sexp_diff in
          let diff = Algo.diff ~original:expected ~updated:actual () in
          let options = Display.Display_options.(create Layout.Single_column) in
          let text = Display.display_with_ansi_colors options diff in
          assert_bool text b
  )

let rec ounit_of_shape (x : _ Shape.t) =
  ounit_of_shape_content x.label x.content

and ounit_of_shape_content label : _ Shape.content -> _ = function
  | Atom t -> label >:: run_test t
  | List xs -> label >::: List.map ounit_of_shape xs

(** Gathers all syntax test files, parses them, then runs the cases they
    contain as tests against TLAPM's syntax parser, skipping or expecting
    failure as appropriate.
*)
let tests =
  Shape.{
      label = "Standardized syntax test corpus";
      content = get_all_tests_under "syntax_corpus";
  }
  |> ounit_of_shape

(** The OUnit2 test entrypoint. *)
let () = run_test_tt_main tests
