open Sexplib

(* [input_sexp] must leave the channel right after the S-expression it returns, so that
   subsequent input functions, including further calls to [input_sexp], see the data
   immediately following it. *)

let with_temp_channel contents ~f =
  let fname, oc = Filename.open_temp_file "test" ".sexp" in
  output_string oc contents;
  close_out oc;
  let ic = open_in_bin fname in
  let finally () =
    close_in ic;
    Sys.remove fname
  in
  match f ic with
  | () -> finally ()
  | exception exn ->
    finally ();
    raise exn
;;

let print_sexps ~input_sexp contents =
  with_temp_channel contents ~f:(fun ic ->
    let rec loop () =
      match input_sexp ic with
      | sexp ->
        Printf.printf "%s, pos_in = %d\n" (Sexp.to_string sexp) (pos_in ic);
        loop ()
      | exception End_of_file -> print_endline "End_of_file"
      | exception Sexp.Parse_error _ -> print_endline "Parse_error"
    in
    loop ())
;;

let test contents =
  Printf.printf "%S\n" contents;
  print_sexps contents ~input_sexp:(fun ic -> Sexp.input_sexp ic)
;;

let%expect_test "unquoted atom followed by another S-expression" =
  test "a(b)";
  [%expect
    {|
    "a(b)"
    a, pos_in = 1
    (b), pos_in = 4
    End_of_file
    |}];
  test "a\"b c\"";
  [%expect
    {|
    "a\"b c\""
    a, pos_in = 1
    "b c", pos_in = 6
    End_of_file
    |}];
  test "a b";
  [%expect
    {|
    "a b"
    a, pos_in = 1
    b, pos_in = 3
    End_of_file
    |}]
;;

let%expect_test "unquoted atom followed by a comment" =
  test "a;c\n";
  [%expect
    {|
    "a;c\n"
    a, pos_in = 1
    End_of_file
    |}];
  test "a#;b c";
  [%expect
    {|
    "a#;b c"
    a#, pos_in = 2
    End_of_file
    |}]
;;

let%expect_test "unquoted atom followed by a stray closing parenthesis" =
  test "a)";
  [%expect
    {|
    "a)"
    a, pos_in = 1
    Parse_error
    |}]
;;

let%expect_test "lists, quoted atoms and atoms at end of input are unaffected" =
  test "(a)b";
  [%expect
    {|
    "(a)b"
    (a), pos_in = 3
    b, pos_in = 4
    End_of_file
    |}];
  test "\"a\"(b)";
  [%expect
    {|
    "\"a\"(b)"
    a, pos_in = 3
    (b), pos_in = 6
    End_of_file
    |}];
  test "a";
  [%expect
    {|
    "a"
    a, pos_in = 1
    End_of_file
    |}]
;;

let%expect_test "other input functions see the data after the atom" =
  with_temp_channel "foo(bar)" ~f:(fun ic ->
    print_endline (Sexp.to_string (Sexp.input_sexp ic));
    print_endline (input_line ic));
  [%expect
    {|
    foo
    (bar)
    |}]
;;

let%expect_test "Annotated.input_sexp" =
  print_sexps "a(b)" ~input_sexp:(fun ic ->
    Sexp.Annotated.get_sexp (Sexp.Annotated.input_sexp ic));
  [%expect
    {|
    a, pos_in = 1
    (b), pos_in = 4
    End_of_file
    |}]
;;
