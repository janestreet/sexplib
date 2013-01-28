open Sexplib
open Test_common
(* TODO: assert that the positions are ok as well *)

let sexp_of_layout_sexp = Sexp.With_layout.sexp_of_t_or_comment

let sexps_of_layout_sexps l = List.map sexp_of_layout_sexp l

let sexp_of_layout_sexps_or_something sexps_with_layout =
  match Sexp.With_layout.Forget.t_or_comments sexps_with_layout with
  | [s] -> s
  | [] | _ :: _ :: _ as sexps ->
    (* this is an error, but returning something is more effective for debug *)
    Sexp.List (
      Sexp.List sexps ::
        Sexp.List (sexps_of_layout_sexps sexps_with_layout) ::
        []
    )

let parsers = [
  Sexp.of_string, "cont";
  (fun s ->
    (* feeding the characters to the parse function one by one to make sure the cont_state
       machinery is working fine *)
    let pos = 0 in
    match Sexp.parse ~len:1 s with
    | Sexp.Done (t, _) -> t
    | Sexp.Cont (cont_state, parse_fun) ->
      let rec aux parse_fun pos =
        assert (pos < String.length s);
        match parse_fun ~pos ~len:1 s with
        | Sexp.Done (t, _) ->
          if pos + 1 = String.length s then t
          else failwith "Should have reached the end1"
        | Sexp.Cont (cont_state, parse_fun) ->
          aux_cont pos cont_state parse_fun
      and aux_cont pos cont_state parse_fun =
        if pos + 1 = String.length s then
          match cont_state with
          | Sexp.Cont_state.Parsing_atom -> begin
            match parse_fun ~pos:0 ~len:1 " " with
            | Sexp.Done (t, _) -> t
            | Sexp.Cont _ -> failwith "Should have reached the end2"
          end
          | Sexp.Cont_state.Parsing_sexp_comment
          | Sexp.Cont_state.Parsing_block_comment
          | Sexp.Cont_state.Parsing_whitespace -> failwith "incomplete"
          | Sexp.Cont_state.Parsing_list -> failwith "Should have reached the end3"
        else aux parse_fun (pos + 1) in
      aux_cont pos cont_state parse_fun
  ), "cont-incremental";
  (fun s -> Sexp.scan_sexp (Lexing.from_string s)), "ocamllex";
  (fun s -> Sexp.Annotated.get_sexp (Sexp.Annotated.of_string s)), "annot";
  (fun s ->
    let sexps_with_layout =
      Sexp.With_layout.Parser.sexps
        Sexp.With_layout.Lexer.main (Lexing.from_string s)
    in
    sexp_of_layout_sexps_or_something sexps_with_layout
  ), "layout-sexps";
  (fun s ->
    let parse =
      let lexbuf = Lexing.from_string s in
      fun () ->
        Sexp.With_layout.Parser.sexp_opt Sexp.With_layout.Lexer.main lexbuf
    in
    let rec aux acc =
      match parse () with
      | None -> List.rev acc
      | Some sexp -> aux (sexp :: acc)
    in
    sexp_of_layout_sexps_or_something (aux [])
  ), "layout-sexp-opt";
]

(* no idea why we can't read multiple sexps from a string directly *)
let put_string_in_channel s f =
  let file, out_channel = Filename.open_temp_file "sexplib_test" "" in
  output_string out_channel s;
  close_out out_channel;
  let in_channel = open_in file in
  let v = try `Value (f in_channel) with exn -> `Exn exn in
  close_in in_channel;
  Sys.remove file;
  match v with
  | `Value v -> v
  | `Exn exn -> raise exn

let list_parsers = [
  (fun s -> Sexp.List (put_string_in_channel s Sexp.input_sexps)), "cont";
  (fun s -> Sexp.List (Sexp.scan_sexps (Lexing.from_string s))), "ocamllex";
  (fun s -> Sexp.List (Sexp.scan_sexps_conv ~f:(fun x -> x) (Lexing.from_string s))), "ocamllex_opt";
  (fun s ->
    Sexp.List (
      put_string_in_channel s (fun ch ->
        List.map Sexp.Annotated.get_sexp (Sexp.Annotated.input_sexps ch)
      )
    )), "annot";
  (fun s ->
    let sexps_with_layout =
      Sexp.With_layout.Parser.sexps
        Sexp.With_layout.Lexer.main (Lexing.from_string s) in
    let sexps = Sexp.With_layout.Forget.t_or_comments sexps_with_layout in
    Sexp.List sexps
  ), "layout-sexps";
]

let tests = ref 0
let failures = ref 0

let same_parse_tree ?no_following_sibling ?(use_list_parsers=false) loc string1 string2 =
  let context_wrappers = wrap_in_context ?no_following_sibling () in
  List.iter (fun context_wrapper ->
    let string1 = context_wrapper string1 in
    let string2 = context_wrapper string2 in
    List.iter (fun (parser_, parser_name) ->
      List.iter (fun (adapt_newline, newline_style) ->
        incr tests;
        try
          let string1 = adapt_newline string1 in
          let string2 = adapt_newline string2 in
          let tree1 = parser_ string1 in
          let tree2 = parser_ string2 in
          if tree1 <> tree2 then (
            incr failures;
            Printf.printf
              "test failure at %s (%s, %s)\n  string1: %S tree1: %s\n  string2: %S tree2: %s\n%!"
              loc parser_name newline_style
              string1 (Sexp.to_string tree1)
              string2 (Sexp.to_string tree2)
          )
        with e ->
          incr failures;
          Printf.printf "test failure at %s (%s, %s, %s) on %S vs %S\n%!"
            loc (Printexc.to_string e) parser_name newline_style string1 string2
      ) newline_adapters
    ) (if use_list_parsers then list_parsers else parsers)
  ) context_wrappers

let same_parse_trees ?no_following_sibling loc string1 string2 =
  same_parse_tree ?no_following_sibling ~use_list_parsers:true loc string1 string2

let parse_fail ?no_following_sibling ?(use_list_parsers=false) loc string f =
  let context_wrappers = wrap_in_context ?no_following_sibling () in
  List.iter (fun context_wrapper ->
    let string = context_wrapper string in
    List.iter (fun (parser_, parser_name) ->
      List.iter (fun (adapt_newline, newline_style) ->
        incr tests;
        try
          let string = adapt_newline string in
          let tree = parser_ string in
          incr failures;
          Printf.printf
            "test failure at %s (%s, %s): should have thrown an exception\nstring: %S tree: %s\n%!"
            loc parser_name newline_style string (Sexp.to_string tree)
        with e ->
          if not (f e) then (
            incr failures;
            Printf.printf "test failure at %s (%s, %s, %s)\n%!"
              loc (Printexc.to_string e) parser_name newline_style
          )
      ) newline_adapters
    ) (if use_list_parsers then list_parsers else parsers)
  ) context_wrappers

let parse_fail_trees ?no_following_sibling loc string f =
  parse_fail ?no_following_sibling ~use_list_parsers:true loc string f

#define _here_ \
    (try assert false; exit 45 \
     with Assert_failure (position, line, col) -> \
       Printf.sprintf "%s:%d:%d" position line col)

let grep pattern string =
  (* hopefully there is no need for escaping *)
  Sys.command ("echo '" ^ string ^ "' | grep -q '" ^ pattern ^ "'") = 0

let () =
  same_parse_tree _here_ "(a)" "(a;\n)"; (* single line comment in a list *)
  same_parse_tree _here_ ";\nb" "b"; (* single line comment at toplevel *)
  same_parse_tree _here_ ";;\nb" "b"; (* single line comment don't nest *)
  same_parse_tree _here_ ";\"\nb" "b"; (* single line comment ignore quotes *)
  same_parse_tree _here_ "(a#)" "(a#;\n)";
  parse_fail _here_ "a#|"
    (function
    | Failure s -> grep "comment tokens in unquoted atom" s
    | Sexp.Parse_error {Sexp.location = "maybe_parse_bad_atom_hash"; err_msg=_; parse_state=_ } -> true
    | _ -> false);
  parse_fail _here_ "a|#"
    (function
    | Failure s -> grep "comment tokens in unquoted atom" s
    | Sexp.Parse_error {Sexp.location = "maybe_parse_bad_atom_pipe"; err_msg=_; parse_state=_ } -> true
    | _ -> false);
  parse_fail _here_ "##|"
    (function
    | Failure s -> grep "comment tokens in unquoted atom" s
    | Sexp.Parse_error {Sexp.location = "maybe_parse_bad_atom_hash"; err_msg=_; parse_state=_ } -> true
    | _ -> false);
  parse_fail _here_ "||#"
    (function
    | Failure s -> grep "comment tokens in unquoted atom" s
    | Sexp.Parse_error {Sexp.location = "maybe_parse_bad_atom_pipe"; err_msg=_; parse_state=_ } -> true
    | _ -> false);
  parse_fail _here_ "#|" (* not terminated *)
    (function
    | Failure s -> grep "incomplete" s || grep "unterminated" s
    | _ -> false);
  parse_fail _here_ "|#" (* not started *)
    (function
    | Failure s -> grep "illegal end of comment" s
    | Sexp.Parse_error {Sexp.location = "maybe_parse_close_comment"; err_msg=_; parse_state=_ } -> true
    | _ -> false);
  parse_fail _here_ ~no_following_sibling:true "#;" (* not followed *)
    (function
    | Sexp.Parse_error _ | Failure _ -> true
    | _ -> false);

  same_parse_tree _here_ "#;a b" "b"; (* sexp comment + atom *)
  same_parse_tree _here_ "#;((a)) b" "b"; (* sexp comment + list *)
  same_parse_tree _here_ "#;\"#;\" b" "b"; (* sexp comment + quoted atom *)
  same_parse_tree _here_ "#;a;comment\nb" "b"; (* sexp comment + single line commment *)
  same_parse_tree _here_ "#;#|aa)|#comment b" "b"; (* sexp comment + block commment *)
  same_parse_tree _here_ "#;a #;(a) #;\"asd\" b" "b"; (* consecutive sexp comment + atom *)
  same_parse_tree _here_ "(#;a #;(a) #;b)" "()"; (* consecutive sexp comment + nothing *)
  same_parse_tree _here_ "#; #; #; comment1 comment2 comment3 a" "a"; (* nested sexp comment *)
  same_parse_tree _here_ "#| ; |# ()" "()"; (* single line comment are not parsed inside of blocks *)
  same_parse_tree _here_ "#|#||#|#a" "a"; (* consecutive comment opening are not parsed
                                             as one invalid atom *)
  (* why do we need a freaking space at the end?? *)
  same_parse_trees _here_ "a #; b c " "a c ";  (* base case, accepting lists *)
  same_parse_trees _here_ "#;#;a b c d " "c d "; (* leading comments work alright *)
  same_parse_trees _here_ "#;#;a b " " "; (* leading comments work alright *)
  same_parse_trees _here_ "plop #;#;a b " "plop "; (* trailing comments work alright *)
  same_parse_trees _here_ "#;b " " "; (* leading comments in front of nothing *)

  (* making sure that '|' is still accepted in literals *)
  same_parse_tree _here_ "(a|b)" "(\"a|b\")";
  same_parse_tree _here_ "(a | b)" "(a \"|\" b)";
  same_parse_tree _here_ "((a)|b)" "((a)\"|b\")";
  same_parse_tree _here_ "(b|(a))" "(\"b|\"(a))";
  same_parse_trees _here_ "a|b " "\"a|b\" ";
  same_parse_trees _here_ "(a)|b " "(a)\"|b\" ";
  same_parse_trees _here_ "b|(a)" "\"b|\"(a)";
  if !failures <> 0 then (
    Printf.printf "%d / %d tests failed\n%!" !failures !tests;
    exit 2
  ) else (
    match Sys.argv with
    | [| _ |] ->
      Printf.printf "Done %d tests\n%!" !tests
    | _ -> ()
  )
