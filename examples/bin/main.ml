open Sexplib

(* An S-expression (or sexp) is a human-readable form for tree-like
   expressions. The Sexplib library provides for parsing and printing such
   structures. Here is the main type:

   type sexp = Atom of string | List of sexp list

   Sexplib is often used in conjuction with ppx_sexp_conv which generates code
   from type definitions to convert OCaml expressions to and from s-expressions.
*)

(* A little example. Some Caribbean islands which contain more than one
   territory. Notice we are using a "local open" with Sexp here to avoid
   writing Sexp.list and Sexp.Atom multple times. *)
let divided_islands =
  Sexp.(List
    [List [Atom "Hispaniola"; List [Atom "Haiti"; Atom "Dominican Republic"]];
     List [Atom "Saint Martin"; List [Atom "Saint Martin"; Atom "Sint Maarten"]]])

(* Print our example sexp. *)
let print_sexp () =
  print_endline "Using Sexp.to_string:";
  print_endline (Sexp.to_string divided_islands);
  print_endline "Using Sexp.to_string_hum (human)";
  print_endline (Sexp.to_string_hum divided_islands)

(* Read from command line, calculating e.g (+ 1 (/ 5 3)) and printing the result. *)
let rec evaluate = function
  | Sexp.(List [Atom "+"; Atom x; Atom y]) -> float_of_string x +. float_of_string y
  | Sexp.(List [Atom "*"; Atom x; Atom y]) -> float_of_string x *. float_of_string y
  | Sexp.(List [Atom "-"; Atom x; Atom y]) -> float_of_string x -. float_of_string y
  | Sexp.(List [Atom "/"; Atom x; Atom y]) -> float_of_string x /. float_of_string y
  | Sexp.(List [Atom _; Atom _; Atom _]) -> failwith "bad operator"
  | Sexp.(List [Atom a; x; Atom y]) -> evaluate (Sexp.List [Atom a; Atom (string_of_float (evaluate x)); Atom y])
  | Sexp.(List [Atom a; Atom x; y]) -> evaluate (Sexp.List [Atom a; Atom x; Atom (string_of_float (evaluate y))])
  | _ -> failwith "unknown sexp"

let calc exp =
  let sexp = Sexp.of_string exp in
    Printf.printf "%f\n" (evaluate sexp)

(* Read from a file, processing, write to another file. *)
let rec rev_sexp = function
  | Sexp.List l -> Sexp.List (List.rev (List.map rev_sexp l))
  | x -> x

let read_write infile outfile =
  let fhi = open_in_bin infile in
  let fho = open_out_bin outfile in
  let sexp = Sexp.input_sexp fhi in
    close_in fhi;
    Sexp.output_hum fho (rev_sexp sexp);
    close_out fho

let () =
  match Sys.argv with
  | [|_; "print_sexp"|] -> print_sexp ()
  | [|_; "calc"; exp|] -> calc exp
  | [|_; "read_write"; infile; outfile|] -> read_write infile outfile
  | _ -> Printf.eprintf "sexplib example: unknown command line\n"
