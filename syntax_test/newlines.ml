open StdLabels
open Sexplib

let windowsify_newlines str =
  let b = Buffer.create (String.length str * 2) in
  for i = 0 to String.length str - 1; do
    match str.[i] with
    | '\n' -> Buffer.add_string b "\r\n"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let () =
  List.iter [
    "simple atom", Sexp.Atom "line1";
    "one trailing newline", Sexp.Atom "line1\n";
    "two lines", Sexp.Atom "line1\nline2";
    "two lines and trailing newline", Sexp.Atom "line1\nline2\n";
    "two lines, windows style", Sexp.Atom "line1\r\nline2";
    "two lines and trailing windows style", Sexp.Atom "line1\r\nline2\r\n";
    "two lines inside of parens", Sexp.of_string "(\"line1\nline2\")";
    "many lines and indentation in the atom", Sexp.Atom "line1\n line2\n  line3\n   line4\n";
    "indentation with tabs in the atom", Sexp.Atom "line1\n\tline2\n\t\tline3\n\t\t\tline4\n";
    "trailing whitespace", Sexp.List [Sexp.List [Sexp.Atom "line1  \n  line3  \n  "]];
    "catalog snapshot", Sexp.Atom "                cancel-buy\n                |  cancel-sell\n                |  |  local-buy\n                |  |  |    local-cancel-buy\n                |  |  |    |  local-cancel-sell\n                |  |  |    |  |  local-sell\nINDEX      buy  |  |  |    |  |  |  sell\n|          |    |  |  |    |  |  |  |\ntest_sym1  10.        10.           9.\ntest_sym3                             \n";
  ] ~f:(fun (title, sexp) ->
    (* My understanding of newlines on windows is that in memory, a newline is a \n, but
       when writing to a file with a file handler open in text mode, all the \n are
       replaced with \r\n. Of course the conversion is undone when reading with a file
       handler open in text mode. If saving \n in text-mode and reading it in binary mode,
       we would receive \r\n back.
       I am testing that serializing + writing in text mode + reading in binary mode +
       deserializing is the identity (because it was before this newline printing stuff),
       and all the other cases should just work. *)
    let reparsed = Sexp.of_string (Sexp.to_string_hum sexp) in
    let reparsed_after_windows_fiddling =
      Sexp.of_string (windowsify_newlines (Sexp.to_string_hum sexp))
    in
    let reparsing_result =
      if reparsed <> sexp then
        Printf.sprintf "to_string_hum + of_string + to_mach is NOT the identity:\n%s\n"
          (Sexp.to_string_mach reparsed)
      else ""
    in
    let reparsing_after_windows_fiddling_result =
      if reparsed_after_windows_fiddling <> sexp then
        Printf.sprintf "to_string_hum + windowsify + of_string + to_mach is NOT the identity:\n%s\n"
          (Sexp.to_string_mach reparsed_after_windows_fiddling)
      else ""
    in
    Printf.printf "------ %s ------\nmach:\n%s\nhum:\n%s\n%s%s\n"
      title
      (Sexp.to_string_mach sexp)
      (Sexp.to_string_hum sexp)
      reparsing_result reparsing_after_windows_fiddling_result
  )
