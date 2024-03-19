open Sexplib
open Expect_test_helpers_core

let%test_module "tests" =
  (module struct
    let same x y = assert (x = y)
    let good s = same (Sexp.of_string s) (Atom "foo")

    let bad s =
      match Sexp.of_string s with
      | exception _exn -> ()
      | _sexp -> failwith "should have failed"
    ;;

    let%test_unit _ = good "foo"
    let%test_unit _ = good "foo\n"
    let%test_unit _ = good "foo;"
    let%test_unit _ = good "foo #;()"
    let%test_unit _ = good "foo #|blah|#"
    let%test_unit _ = good "foo #|blah|#\n"
    let%test_unit _ = good "foo; blah"
    let%test_unit _ = good "foo; blah\n"
    let%test_unit _ = good "foo; blah\n"
    let%test_unit _ = good "foo #; blah "
    let%test_unit _ = good "foo #; blah\n"
    let%test_unit _ = good "foo #; ()"

    (* regression test, these strings used to fail to parse *)
    let%test_unit _ = good "foo #; blah"
    let%test_unit _ = good "foo #; blah #; blah"
    let%test_unit _ = good "foo #; #; blah blah"
    let%test_unit _ = good "foo #; blah#"

    (* multiple sexps *)
    let%test_unit _ = bad "foo bar"

    (* unterminated block comment *)
    let%test_unit _ = bad "foo #| bar"

    (* unterminated sexp *)
    let%test_unit _ = bad "foo ("

    let%expect_test "of_string_many" =
      let test str =
        let sexps = Sexp.of_string_many str in
        List.iter print_s sexps
      in
      test "(foo) (bar)";
      [%expect {|
        (foo)
        (bar)
        |}]
    ;;

    let%expect_test "of_string_many_conv_exn" =
      let module Foo = struct
        type t =
          | A
          | B
        [@@deriving sexp]
      end
      in
      let list_of_sexp = Conv.list_of_sexp in
      let sexp_of_list = Conv.sexp_of_list in
      let test str =
        let foos = Sexp.of_string_many_conv_exn str [%of_sexp: Foo.t list] in
        List.iter (fun (x : Foo.t list) -> print_s ([%sexp_of: Foo.t list] x)) foos
      in
      test "(A) (B)";
      [%expect {|
        (A)
        (B)
        |}];
      show_raise (fun () -> test "(A) (B) (C)");
      [%expect
        {|
        (raised (
          Of_sexp_error
          "test_sexp_of_string.ml.t_of_sexp: unexpected variant constructor"
          (invalid_sexp C)
          (containing_sexp (C))))
        |}]
    ;;
  end)
;;

let%test_module "Annotated" =
  (module struct
    let%expect_test "of_string" =
      let good s =
        assert (Sexp.Annotated.get_sexp (Sexp.Annotated.of_string s) = Atom "foo")
      in
      let bad s = show_raise (fun () -> Sexp.Annotated.of_string s) in
      good "foo";
      good "foo\n";
      good "foo;";
      good "foo #;()";
      good "foo #|blah|#";
      good "foo #|blah|#\n";
      good "foo; blah";
      good "foo; blah\n";
      good "foo; blah\n";
      (* multiple sexps *)
      bad "foo bar";
      [%expect
        {|
        (raised (
          Failure
          "Sexplib.Sexp.Annotated.of_string: got multiple S-expressions where only one was expected."))
        |}];
      (* unterminated block comment *)
      bad "foo #| bar";
      [%expect
        {|
        (raised (
          Failure
          "Sexplib.Sexp.Annotated.of_string: S-expression followed by data at position 3..."))
        |}];
      (* unterminated sexp *)
      bad "(foo";
      [%expect
        {|
        (raised (
          Failure
          "Sexplib.Sexp.Annotated.of_string: incomplete S-expression while in state Parsing_list: (foo"))
        |}]
    ;;

    let%expect_test "of_string_many" =
      let test str =
        let annotated_sexps = Sexp.Annotated.of_string_many str in
        List.iter (fun x -> print_s (Sexp.Annotated.get_sexp x)) annotated_sexps
      in
      test "(foo) (bar)";
      [%expect {|
        (foo)
        (bar)
        |}];
      show_raise (fun () -> test "(foo) (bar");
      [%expect
        {|
        (raised (
          parse_error.ml.Parse_error (
            (position (
              (line   1)
              (col    10)
              (offset 10)))
            (message "unclosed parentheses at end of input"))))
        |}]
    ;;
  end)
;;
