open Sexplib
open Sexplib.Conv
open Printf

module type Load = sig
  val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a
  val load_sexps_conv    : string -> (Sexp.t -> 'a) -> 'a Sexp.Annotated.conv list
end

exception E of [ `Expected of Sexp.t ] * [ `Got of Sexp.t ] with sexp

let () =
  Printexc.register_printer (fun exc ->
    match Sexplib.Conv.sexp_of_exn_opt exc with
    | None -> None
    | Some sexp ->
      Some (Sexp.to_string_hum ~indent:2 sexp))

let command_exn str =
  match Sys.command str with
  | 0 -> ()
  | code -> failwith (sprintf "command %S exited with code %d" str code)

module Make (Load : Load) = struct
  module Macro = struct end (* shadowing Macro to avoid mistakenly
                               calling it instead of Load *)
  let test_id = ref 0

  let id x = x

  let with_files files ~f =
    let time = Unix.time () in
    incr test_id;
    let dir =
      sprintf "%s/macros-test/macros-test-%f-%d"
        (Filename.get_temp_dir_name ()) time !test_id
    in
    List.iter (fun (file, contents) ->
      let file_dir = Filename.concat dir (Filename.dirname file) in
      command_exn ("mkdir -p " ^ file_dir);
      let out_channel = open_out (Filename.concat dir file) in
      output_string out_channel (contents ^ "\n");
      close_out out_channel)
      files;
    let tear_down () =
      command_exn ("rm -rf -- " ^ dir);
    in
    try let v = f dir in tear_down (); v
    with e -> tear_down (); raise e

  let check files expected =
    with_files files ~f:(fun dir ->
      let actual =
        Load.load_sexp_conv_exn (Filename.concat dir "input.sexp") id
      in
      let expected = Sexp.of_string expected in
      if actual <> expected then
        raise (E (`Expected expected, `Got actual))
    )

  (* Not quite the same as [Str] functions because it reapplies itself, see the
     use below to eliminate "/./././...". *)
  let replace ~sub ~by str =
    let rec loop str i =
      if i + String.length sub < String.length str then
        if String.sub str i (String.length sub) = sub then
          let str =
            String.sub str 0 i ^
            by ^
            String.sub str (i + String.length sub) (String.length str - i - String.length sub)
          in
          loop str i
        else loop str (i + 1)
      else str
    in loop str 0

  let contains str ~sub =
    Str.string_match (Str.regexp (sprintf ".*%s.*" (Str.quote sub))) str 0

  let normalize str = try Sexp.to_string (Sexp.of_string str) with _ -> str

  type try_parse_string = string
  let sexp_of_try_parse_string str =
    try Sexp.of_string str with _ -> sexp_of_string str
  exception Wrong_error of exn * [ `Expected_to_contain of try_parse_string ] with sexp

  let check_error ?f ~expect files =
    with_files files ~f:(fun dir ->
      let file = Filename.concat dir "input.sexp" in
      let error_kind =
        try
          match f with
          | Some f -> ignore (Load.load_sexp_conv_exn file f); `No_error
          | None -> ignore (Load.load_sexp_conv_exn file id); `No_error
        with e -> `Error e
      in
      match error_kind with
      | `Error e ->
        List.iter (fun expect ->
          let expect = replace ~sub:"DIR" ~by:dir expect in
          let str = replace ~sub:"/./" ~by:"/" (Printexc.to_string e) in
          if not (contains (normalize str) ~sub:(normalize expect)) then
            raise (Wrong_error (e, `Expected_to_contain expect)))
          expect
      | `No_error ->
        failwith
          (sprintf
             "File %s expected to throw an exception, but loaded successfully."
             file))

  let check_error_count ~f files ~expected_count =
    with_files files ~f:(fun dir ->
      let file = Filename.concat dir "input.sexp" in
      let results = Load.load_sexps_conv file f in
      let rec count = function
        | `Error _ :: xs -> 1 + count xs
        | `Result _ :: xs -> count xs
        | [] -> 0
      in
      let actual_count = count results in
      if actual_count <> expected_count then
        failwith
          (sprintf "Expected %d errors, got %d." expected_count actual_count))

  TEST_UNIT "simple" =
    check
      [ "input.sexp"
      , "(:include defs.sexp)
         ((field1 value1)
         (field2 ((:include include.sexp) 0004 0005))
         (field3 (:concat a (:use f (x (:use x))))))"

      ; "defs.sexp"
      , "(:let x () y z)
         (:let f (x) (:concat (:use x) (:use x)))"

      ; "include.sexp"
      , "0001 0002 0003" ]

      "((field1 value1)
       (field2 (0001 0002 0003 0004 0005))
       (field3 ayzyz))"

 TEST_UNIT "include chain with subdirectories" =
    check
      [ "input.sexp"
      , "(:include include/a.sexp)"

      ; "include/a.sexp"
      , "(:include b.sexp)"

      ; "include/b.sexp"
      , "(this is include/b)" ]

      "(this is include/b)"

  TEST_UNIT "hello world" =
    check
      [ "input.sexp"
      , "(:include defs.sexp)
         (:include template.sexp)
         (:use f (a (:use a)) (b (:use b)))"

      ; "defs.sexp"
      , "(:let a () hello)
         (:let b () \" world\")"

      ; "template.sexp"
      , "(:let f (a b) (:concat (:use a) (:use b)))" ]

      "\"hello world\""

  TEST_UNIT "nested let" =
    check
      [ "input.sexp"
      , "(:let f (x)
           (:let g (y)
              (:use y) (:use y))
           (:use g (y (:use x))) (:use g (y (:use x))))
         (:concat (:use f (x x)))" ]
      "xxxx"

  TEST_UNIT "argument list scoping" =
    check
      [ "input.sexp"
      , "(:let a () a)
         (:let b () b)
         (:let f (b a) (:concat (:use b) (:use a)))
         (:use f (b (:use a)) (a (:use b)))" ]
      "ab"

  TEST_UNIT "empty argument" =
    check
      [ "input.sexp"
      , "(:let f (x) (:use x) bla)
         (:use f (x))" ]
      "bla"

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:let f (()) foo)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
         (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:1:9
          (Failure \"Error evaluating macros: Atom expected\"))
         ())"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:let f x foo)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
         (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:1:8
          (Failure \"Error evaluating macros: Atom list expected\"))
         x)"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:concat :use x)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
         (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:1:9
          (Failure \"Error evaluating macros: Unexpected :use\"))
         :use)"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:let f (x) (:use x))
         (:use f (()))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
         (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:2:17
          (Failure \"Error evaluating macros: Malformed argument\"))
         (()))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:let f (x) (:use x))
         (:use f (y x))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
         (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:2:9
          (Failure \"Error evaluating macros: Formal args of f differ from supplied args, formal args are [x]\"))
         (:use f (y x)))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:let f (a) body of f)
         (:use f (a a))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn
          DIR/input.sexp:1:0
          (Failure \"Error evaluating macros: Unused variables: a\"))
        (:let f (a) body of f))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:let f (a a) (:concat (:use a) (:use a)))
         (:use f (a foo) (a foo))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn
          DIR/input.sexp:1:0
          (Failure \"Error evaluating macros: Duplicated let argument: a\"))
        (:let f (a a) (:concat (:use a) (:use a))))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)
         (:use f (x bla))"

      ; "include.sexp"
      , "(:let y () bla)
         (:let f (x)
             ((:use x) (:use y)))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
                (Sexplib.Sexp.Annotated.Conv_exn
                DIR/include.sexp:2:9
                (Failure \"Error evaluating macros: Undeclared arguments in let: y\"))
                (:let f (x) ((:use x) (:use y))))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:let x () x)
         (:include include.sexp)"

      ; "include.sexp"
      , "(:use x)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn
          DIR/include.sexp:1:6
          (Failure
            \"Error evaluating macros: Undefined variable (included files cannot reference variables from outside)\"))
        x)"]

  TEST_UNIT ":include can cause variable capture" =
    check
      [ "input.sexp"
      , "(:let x () 2)
         (:include include.sexp)
         (:use x)"

      ; "include.sexp"
      , "(:let x () 1)" ]
      "1"

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:concat (a b))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
          (Sexplib.Sexp.Annotated.Conv_exn
            DIR/input.sexp:1:0
            (Failure \"Error evaluating macros: Malformed concat application: (:concat(a b))\"))
          (:concat (a b)))"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)
         (:use f (a ()))"

      ; "include.sexp"
      , "(:let f (a)
           (:concat (:use a)))" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
          (Sexplib.Sexp.Annotated.Conv_exn
            DIR/include.sexp:2:11
            (Failure \"Error evaluating macros: Malformed concat application: (:concat())\"))
          (:concat (:use a)))"]

  TEST_UNIT "correct error location in a nested let" =
    check_error
      [ "input.sexp"
      , "(:let f ()
           (:let g () (:let incorrect))
           (:use g))
         (:use f)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
          (Sexplib.Sexp.Annotated.Conv_exn
             DIR/input.sexp:2:23
             (Failure \"Error evaluating macros: Unexpected :let\"))
             :let)"]

  TEST_UNIT "correct location with chains of includes" =
    check_error
      [ "input.sexp"
      , "(:include a)"

      ; "a"
      , "(:include b)"

      ; "b"
      , "something invalid like :concat" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
        (Sexplib.Sexp.Annotated.Conv_exn
          DIR/b:1:23
          (Failure \"Error evaluating macros: Unexpected :concat\"))
        :concat)"]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "\n(:let f ())" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
          (Sexplib.Sexp.Annotated.Conv_exn
            DIR/input.sexp:2:0
            (Failure \"Error evaluating macros: Empty let bodies not allowed\"))
          (:let f ()))"]

  exception Conv_error

  let rec conv_error = function
    | Sexp.Atom "error" as t ->
      raise (Pre_sexp.Of_sexp_error (Conv_error, t))
    | Sexp.Atom _ -> ()
    | Sexp.List ts -> List.iter conv_error ts

  TEST_UNIT "error location for conversion errors" =
    check_error ~f:conv_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(foo bar error)" ]
      ~expect:["(Sexplib.Conv.Of_sexp_error
          (Sexplib.Sexp.Annotated.Conv_exn
            DIR/include.sexp:1:9
            (\"Test_macros.Make(Load).Conv_error\"))
          error)"]

  TEST_UNIT "multiple conversion errors" =
    check_error_count ~f:conv_error ~expected_count:2
      [ "input.sexp"
      , "(:include include.sexp) (:include include.sexp)"

      ; "include.sexp"
      , "(foo bar error)" ]

  TEST_UNIT =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:include include.sexp)" ]
      ~expect:["Macro.Include_loop_detected(\"DIR/include.sexp\")"]

  (* The exact form of the error messages below will depend on whether we are
     using the sexplib or the async sexp reader, so we only match on relevant
     expression parts. *)

  (* what stops this loop is that the filenames become too long. We have to rewrite the
     error messages since the exact number of "./" in the path depends on the limit on
     path length. *)
  TEST_UNIT "sneaky include loop" =
    check_error
      [ "input.sexp"
      , "(:include include.sexp)"

      ; "include.sexp"
      , "(:include ././include.sexp)" ]
      ~expect:["DIR/include.sexp"; "File name too long"]

  TEST_UNIT "parsing error 1" =
    check_error
      [ "input.sexp"
      , "(:include include.sexp) ()"

      ; "include.sexp"
      , ")" ]
      ~expect:["DIR/include.sexp"; "unexpected character: ')'"]

  TEST_UNIT "parsing error 2" =
    check_error
      [ "input.sexp"
      , "(:include include.sexp) ()"

      ; "include.sexp"
      , "(" ]
      ~expect:["DIR/include.sexp"]
end

module M = Make (Macro)

TEST_UNIT =
  match Macro.expand_local_macros [Sexp.of_string "(:use x)"] with
  | `Result _ -> assert false
  | `Error (e, _) ->
    let expected =
      "(Failure\"Error evaluating macros: Undefined variable\")"
    in
    if Sexp.to_string (sexp_of_exn e) <> expected then raise e
;;
