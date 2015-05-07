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

module Field_name_should_not_be_rewritten = struct
  type nonrec r = { r : r }
  let _ (r : r) = r.r
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

  module Include = struct
    include (struct
      type t = int with sexp
    end : sig
      type t with sexp
    end with type t := int)
  end
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

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] with sexp
  type 'a u = [ t | `b of 'a ] * int with sexp
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

module No_unused_rec_warning = struct
  type r = { field : r -> unit }
  with sexp_of
end

module True_and_false = struct
  type t =
  | True
  | False
  with sexp

  let () = assert (Sexp.to_string (sexp_of_t True) = "True")
  let () = assert (Sexp.to_string (sexp_of_t False) = "False")
  let () = assert (True = t_of_sexp (Sexp.of_string "True"))
  let () = assert (False = t_of_sexp (Sexp.of_string "False"))
  let () = assert (True = t_of_sexp (Sexp.of_string "true"))
  let () = assert (False = t_of_sexp (Sexp.of_string "false"))

  type u =
  | True of int
  | False of int
  with sexp

  let () = assert (Sexp.to_string (sexp_of_u (True 1)) = "(True 1)")
  let () = assert (Sexp.to_string (sexp_of_u (False 2)) = "(False 2)")
  let () = assert (True 1 = u_of_sexp (Sexp.of_string "(True 1)"))
  let () = assert (False 2 = u_of_sexp (Sexp.of_string "(False 2)"))
  let () = assert (True 1 = u_of_sexp (Sexp.of_string "(true 1)"))
  let () = assert (False 2 = u_of_sexp (Sexp.of_string "(false 2)"))

  exception True with sexp
  let () = assert ("pa_sexp_test.ml.True_and_false.True" = Sexp.to_string (sexp_of_exn True))

  exception False of int with sexp
  let () = assert ("(pa_sexp_test.ml.True_and_false.False 1)" = Sexp.to_string (sexp_of_exn (False 1)))

end

module Gadt = struct
  let is_eq sexp str =
    let sexp2 = Sexp.of_string str in
    if sexp <> sexp2 then begin
      Printf.printf "%S vs %S\n%!" (Sexp.to_string sexp) str;
      assert false
    end

  (* plain type without argument *)
  type 'a s = Packed : 'a s with sexp_of
  let () = is_eq (<:sexp_of< int s >> Packed) "Packed"

  (* two kind of existential variables *)
  type 'a t = Packed : 'a * _ * 'b sexp_opaque -> 'a t with sexp_of
  let () = is_eq (<:sexp_of< int t >> (Packed (2, "asd", 1.))) "(Packed 2 _ <opaque>)"

  (* plain type with argument *)
  type 'a u = A : 'a -> 'a u with sexp_of
  let () = is_eq (<:sexp_of< int u >> (A 2)) "(A 2)"

  (* recursive *)
  type v = A : v option -> v with sexp_of
  let () = is_eq (<:sexp_of< v >> (A (Some (A None)))) "(A((A())))"

  (* implicit existential variable *)
  type w = A : 'a * int * ('a -> string) -> w with sexp_of
  let () = is_eq (<:sexp_of< w >> (A (1., 2, string_of_float))) "(A _ 2 <fun>)"

  (* tricky variable naming *)
  type 'a x = A : 'a -> 'b x with sexp_of
  let () = is_eq (<:sexp_of< int x >> (A 1.)) "(A _)"

  (* unused but colliding variables *)
  type (_, _) y = A : ('a, 'a) y with sexp_of
  let () = is_eq (<:sexp_of< (int, int) y >> A) "A"

  (* making sure we're not reversing parameters *)
  type (_, _) z = A : ('a * 'b) -> ('a, 'b) z with sexp_of
  let () = is_eq (<:sexp_of< (int, string) z >> (A (1, "a"))) "(A (1 a))"

end

module Anonymous_variable = struct
  type _ t = int with sexp
  let () = assert (Sexp.to_string (<:sexp_of< _ t >> 2) = "2")
  let () = assert (<:of_sexp< _ t >> (Sexp.of_string "2") = 2)
end

module Record_field_disambiguation = struct

  type a = { fl: float; b : b }
  and b = { fl: int }
  with sexp

end

module Private = struct
  type t = private int with sexp_of

  type ('a, 'b) u = private t with sexp_of

  type ('a, 'b, 'c) v = private ('a, 'b) u with sexp_of
end
