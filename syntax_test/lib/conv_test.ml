open Sexplib.Conv

module Exceptions = struct

  let check_sexp exn string =
    match sexp_of_exn_opt exn with
    | None -> raise exn
    | Some sexp ->
      let sexp_as_string = Sexplib.Sexp.to_string sexp in
      if sexp_as_string <> string
      then failwith sexp_as_string

  (* first global exceptions, checking different arities since they
     don't have the same representation *)
  exception Arg0 with sexp
  exception Arg1 of int with sexp
  exception Arg2 of int * int with sexp
  TEST_UNIT = check_sexp Arg0 "conv_test.ml.Exceptions.Arg0"
  TEST_UNIT = check_sexp (Arg1 1) "(conv_test.ml.Exceptions.Arg1 1)"
  TEST_UNIT = check_sexp (Arg2 (2, 3)) "(conv_test.ml.Exceptions.Arg2 2 3)"

  (* now local exceptions *)
  let exn (type a) a sexp_of_a =
    let module M = struct exception E of a with sexp end in
    M.E a

  TEST_UNIT "incompatible exceptions with the same name" =
    let e_int = exn 1 sexp_of_int in
    let e_string = exn "a" sexp_of_string in
    check_sexp e_int "(conv_test.ml.Exceptions.E 1)";
    check_sexp e_string "(conv_test.ml.Exceptions.E a)"

  let copy (type a) (e : a) : a =
    Obj.obj (Obj.dup (Obj.repr e))

  TEST "sexp converters are finalized properly for local exceptions" =
    let e = exn 2.5 sexp_of_float in
    let copy_of_e = copy e in
    let e_finalized = ref false in
    let ref_for_copy_of_e = ref None in
    Gc.finalise (fun _ -> e_finalized := true) e;
    Gc.finalise (fun e -> ref_for_copy_of_e := Some e) copy_of_e;
    check_sexp e         "(conv_test.ml.Exceptions.E 2.5)";
    check_sexp copy_of_e "(conv_test.ml.Exceptions.E 2.5)";
    Gc.compact ();
    assert !e_finalized;
    check_sexp copy_of_e "(conv_test.ml.Exceptions.E 2.5)";
    Gc.compact ();
    match !ref_for_copy_of_e with
    | None -> failwith "finalizer should have run"
    | Some e -> sexp_of_exn_opt e = None
end
