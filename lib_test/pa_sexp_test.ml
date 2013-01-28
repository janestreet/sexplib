open Sexplib
open Sexp
open Conv

let debug = ref false
type ('a, 'b) variant2 = 'a with of_sexp
type variant3 = [ `B | ([ `C ], int) variant2 ] with of_sexp

let () =
  let not_deserializable = Atom "C" in
  try ignore (<:of_sexp< variant3 >> not_deserializable);
      failwith "Expected an exception about a silly type"
  with Conv.Of_sexp_error (exn, sexp) ->
    if !debug then (
      let sexp1 = Conv.sexp_of_exn exn in
      Printf.printf "Conv_error.Of_sexp_error (%s, %s)\n%!"
        (Sexp.to_string sexp1)
        (Sexp.to_string sexp)
    )

(* this one would trigger a warning in 4.0 about unused rec if type_conv
   says that this definition is recursive *)
type r = { r : int } with sexp

module A = struct
  type nonrec r = { r : r }
  let _ (r : r) = r.r (* checking that the field is not rewritten *)
end
