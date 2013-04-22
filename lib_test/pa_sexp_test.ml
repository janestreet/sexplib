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

module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] with sexp
  end = struct
    type t = [ `A ] with sexp
  end
  module Empty = struct
  end
  module No_warning2(X : sig type t with sexp end) = struct
  end
  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with sexp end) = Empty *)
  module type S = sig
    type t = [ `A ] with sexp
  end
  module No_warning4 : S = struct
    type t = [ `A ] with sexp
  end
  module No_warning5 : S = ((struct
    type t = [ `A ] with sexp
  end : S) : S)

  module Nested_functors
    (M1 : sig type t with sexp end)
    (M2 : sig type t with sexp end) = struct
  end

  let () =
    let module M : sig
      type t with sexp
    end = struct
      type t with sexp
    end in
    ()
end

module Default = struct
  type t = {
    a : int with default(2);
  } with sexp
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List [List [Atom "a"; Atom "2"]]) = sexp_of_t { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the sexp_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = ([`A] as 'a) constraint 'a = a
    with sexp
  end = struct
    type a = [ `A ] with sexp
    type t = [ `A ] with sexp
  end
  let () =
    assert (Sexp.to_string (B.sexp_of_t `A) = "A");
    assert (`A = B.t_of_sexp (Sexp.of_string "A"));
    ()

  module B2 = struct
    type t = [ B.t | `B ] with sexp
  end

  module C : sig
    type t = (int as 'a)
    with sexp
  end = struct
    type t = int
    with sexp
  end

  module D : sig
    type t = 'a constraint 'a = int
    with sexp
  end = struct
    type t = int
    with sexp
  end
end

module Drop_default = struct
  type t = {
    a : int with default(2), sexp_drop_default;
  } with sexp
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })
end

module Drop_if = struct
  type t = {
    a : int with default(2), sexp_drop_if(fun x -> x mod 2 = 0)
  } with sexp
  let () = assert (Sexp.(List [List [Atom "a"; Atom "1"]]) = sexp_of_t { a = 1 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 2 })
  let () = assert (Sexp.(List [List [Atom "a"; Atom "3"]]) = sexp_of_t { a = 3 })
  let () = assert (Sexp.(List []) = sexp_of_t { a = 4 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "1"]])) = { a = 1 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "2"]])) = { a = 2 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "3"]])) = { a = 3 })
  let () = assert (t_of_sexp (Sexp.(List [List [Atom "a"; Atom "4"]])) = { a = 4 })
  let () = assert (t_of_sexp (Sexp.(List [])) = { a = 2 })

  type u = {
    a : int with sexp_drop_if(fun x ->
      (* pa_type_conv used to drop parens altogether, causing type errors in the
         following code *)
      let pair = (x, 2) in
      match Some pair with
      | None -> true
      | Some (x, y) -> x = y
    )
  } with sexp
end

module B = struct
  (* checking that there is no warning about 'unused rec'  *)
  type r = { field : r -> unit }
  with sexp_of
end
