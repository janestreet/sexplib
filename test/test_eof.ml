open Sexplib.Std

let with_temp_file sexp_of_t ~contents ~f =
  let fname, oc = Filename.open_temp_file "test" ".sexp" in
  let result =
    match
      output_string oc contents;
      close_out oc;
      f fname
    with
    | x -> `Ok x
    | exception e -> `Error e
  in
  Sys.remove fname;
  Printf.printf !"%{sexp:[ `Ok of t | `Error of exn ]}" result
;;

let%expect_test "file ending with an atom" =
  with_temp_file [%sexp_of: int] ~contents:"5" ~f:(fun fname ->
    Sexplib.Sexp.load_sexp_conv_exn fname [%of_sexp: int]);
  [%expect {| (Ok 5) |}]
;;

let%expect_test "file ending with an atom" =
  with_temp_file [%sexp_of: Sexplib.Sexp.t list] ~contents:"5" ~f:(fun fname ->
    let ic = open_in fname in
    match Sexplib.Sexp.input_sexps ic with
    | x ->
      close_in ic;
      x
    | exception e ->
      close_in ic;
      raise e);
  [%expect {| (Ok (5)) |}]
;;

let%expect_test "strict [load_sexp] at end of file after the first sexp" =
  let test ?buf contents =
    with_temp_file [%sexp_of: Sexplib.Sexp.t] ~contents ~f:(fun fname ->
      let load f =
        try f fname with
        | Failure msg ->
          failwith (Base.String.substr_replace_all msg ~pattern:fname ~with_:"FILE")
      in
      let sexp = load (Sexplib.Sexp.load_sexp ?buf) in
      let annotated = load (Sexplib.Sexp.Annotated.load_sexp ?buf) in
      assert (Sexplib.Sexp.equal (Sexplib.Sexp.Annotated.get_sexp annotated) sexp);
      let converted =
        load (fun fname -> Sexplib.Sexp.load_sexp_conv_exn ?buf fname Fun.id)
      in
      assert (Sexplib.Sexp.equal converted sexp);
      sexp);
    print_newline ()
  in
  (* The file ends with a sexp comment of an atom, with no trailing whitespace. *)
  test "(a b) #;c";
  test "a #;b";
  test "(a b)\n#; #; c d";
  test ~buf:(Bytes.create 1) "(a b) #;c";
  test "(a b) #;c\n";
  [%expect
    {|
    (Ok (a b))
    (Ok a)
    (Ok (a b))
    (Ok (a b))
    (Ok (a b))
    |}];
  (* The file ends with a second sexp, an atom with no trailing whitespace. *)
  test "(a b) c";
  test ~buf:(Bytes.create 1) "(a b) c";
  [%expect
    {|
    (Error
     (Failure
      "Sexplib.Sexp.gen_load_sexp: more than one S-expression in file FILE"))
    (Error
     (Failure
      "Sexplib.Sexp.gen_load_sexp: more than one S-expression in file FILE"))
    |}];
  (* The file ends with incomplete data. *)
  test "(a b) #;";
  test "(a b) (c";
  [%expect
    {|
    (Error
     (Failure
      "Sexplib.Sexp.gen_load_sexp: additional incomplete data in state Parsing_sexp_comment loading file FILE"))
    (Error
     (Failure
      "Sexplib.Sexp.gen_load_sexp: additional incomplete data in state Parsing_atom loading file FILE"))
    |}];
  (* The file ends with a carriage return that no newline follows. *)
  test "(a b) #;c\r";
  [%expect
    {|
    (Error
     (Sexplib.Sexp.Parse_error
      ((err_msg "unexpected character after carriage return") (text_line 1)
       (text_char 10) (global_offset 10) (buf_pos 0))))
    |}]
;;
