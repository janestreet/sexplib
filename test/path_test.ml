open Sexplib
open Conv

type foo =
  | A
  | B of int * float
[@@deriving sexp]

type 'a t =
  { x : foo
  ; foo : int
  ; bar : (float * string) list option
  }
[@@deriving sexp]

type u = { t : int t } [@@deriving sexp]

let%test_unit _ =
  let t = { x = B (42, 3.1); foo = 3; bar = Some [ 3.1, "foo" ] } in
  let u = { t } in
  let u_sexp = sexp_of_u u in
  assert (Sexp.to_string u_sexp = "((t((x(B 42 3.1))(foo 3)(bar(((3.1 foo)))))))");
  let path_str = ".[0].[1]" in
  let path = Path.parse path_str in
  let subst, el = Path.subst_path u_sexp path in
  assert (Sexp.to_string el = "((x(B 42 3.1))(foo 3)(bar(((3.1 foo)))))");
  let dumb_sexp = subst (Atom "SUBST1") in
  assert (Sexp.to_string dumb_sexp = "((t SUBST1))");
  let path_str = ".t.x.B[1]" in
  let path = Path.parse path_str in
  let subst, el = Path.subst_path u_sexp path in
  assert (Sexp.to_string el = "3.1");
  let u_sexp = subst (Atom "SUBST2") in
  assert (Sexp.to_string u_sexp = "((t((x(B 42 SUBST2))(foo 3)(bar(((3.1 foo)))))))")
;;

let%expect_test "Path.parse with escape characters" =
  let module Path = struct
    include Path

    type nonrec el = el =
      | Pos of int
      | Match of string * int
      | Rec of string
    [@@deriving sexp_of]

    type t = el list [@@deriving sexp_of]
  end
  in
  let test ?cr string =
    Expect_test_helpers_core.require_does_not_raise ?cr (fun () ->
      Stdio.print_s (Path.sexp_of_t (Path.parse string)))
  in
  (* try escaping . *)
  test {|.github\.com|};
  [%expect {| ((Rec github.com)) |}];
  (* try escaping brackets *)
  test {|.array\[0\]|};
  [%expect {| ((Rec array[0])) |}];
  (* try escaping \ *)
  test {|.a\\b|};
  [%expect {| ((Rec "a\\b")) |}]
;;

let%expect_test "substitution with escape characters in path" =
  let test ?cr path_string sexp_string =
    Expect_test_helpers_core.require_does_not_raise ?cr (fun () ->
      let path = Path.parse path_string in
      let sexp = Sexp.of_string sexp_string in
      let subst, sub = Path.subst_path sexp path in
      let subst = subst (Atom "$SUBST") in
      Stdio.print_s [%sexp { subst : Sexp.t; sub : Sexp.t }])
  in
  test {|.github\.com|} "((github.com (127.0.0.1 192.168.0.1)))";
  [%expect {| ((subst ((github.com $SUBST))) (sub (127.0.0.1 192.168.0.1))) |}]
;;
