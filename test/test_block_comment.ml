open! Base
open Sexplib
open Expect_test_helpers_core

let print_scanned_sexps str =
  List.iter (Sexp.scan_sexps (Lexing.from_string str)) ~f:print_s
;;

let%expect_test "quoted atom after a block comment" =
  print_scanned_sexps {|#|"a"|# "b"|};
  [%expect {| b |}]
;;

let%expect_test "quoted atom after a block comment in a list" =
  print_scanned_sexps {|(#| "x" |# "y" z)|};
  [%expect {| (y z) |}]
;;

let%expect_test "escaped string in a block comment" =
  print_scanned_sexps {|#|"a\"x"|# "b"|};
  [%expect {| b |}]
;;

let%expect_test "quoted string in a nested block comment" =
  print_scanned_sexps {|#| #| "a" |# |# "b"|};
  [%expect {| b |}]
;;

let%expect_test "block comments through the string parsers" =
  List.iter [ {|#|"a"|# "b"|}; {|(#| "x" |# "y" z)|} ] ~f:(fun str ->
    print_s (Sexp.of_string str);
    List.iter (Sexp.of_string_many str) ~f:print_s);
  [%expect
    {|
    b
    b
    (y z)
    (y z)
    |}]
;;

let%expect_test "block comment text and quoted atom are preserved with layout" =
  let module M = Sexp.With_layout in
  let sexps = M.Parser.sexps M.Lexer.main (Lexing.from_string {|#|"a"|# "b"|}) in
  List.iter sexps ~f:(fun sexp -> print_s (M.sexp_of_t_or_comment sexp));
  [%expect
    {|
    (Comment (Plain_comment 0:0 "#|\"a\"|#"))
    (Sexp (Atom 0:8 b ("\"b\"")))
    |}]
;;
