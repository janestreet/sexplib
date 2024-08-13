open! Base
open! Expect_test_helpers_core
open Sexplib
open Sexp_grammar_validation

let test m = validate_grammar m |> require_ok

module Test = struct
  include Sexplib0.Sexp_conv

  type mat = Conv.mat

  let mat_sexp_grammar = Conv.mat_sexp_grammar
  let mat_of_sexp = Conv.mat_of_sexp
  let sexp_of_mat = Conv.sexp_of_mat
  let compare_mat = Poly.compare
  let quickcheck_generator_mat = Base_quickcheck.Generator.float64_mat
  let quickcheck_shrinker_mat = Base_quickcheck.Shrinker.float64_mat
  let quickcheck_observer_mat = Base_quickcheck.Observer.float64_mat

  let%expect_test "[mat]" =
    test
      (module struct
        type t = mat [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Cons Integer (Cons Integer (Many Float)))) |}]
  ;;

  type float64_mat = Conv.float64_mat

  let float64_mat_sexp_grammar = Conv.float64_mat_sexp_grammar
  let float64_mat_of_sexp = Conv.float64_mat_of_sexp
  let sexp_of_float64_mat = Conv.sexp_of_float64_mat
  let compare_float64_mat = Poly.compare
  let quickcheck_generator_float64_mat = Base_quickcheck.Generator.float64_mat
  let quickcheck_shrinker_float64_mat = Base_quickcheck.Shrinker.float64_mat
  let quickcheck_observer_float64_mat = Base_quickcheck.Observer.float64_mat

  let%expect_test "[float64_mat]" =
    test
      (module struct
        type t = float64_mat [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Cons Integer (Cons Integer (Many Float)))) |}]
  ;;

  type float32_mat = Conv.float32_mat

  let float32_mat_sexp_grammar = Conv.float32_mat_sexp_grammar
  let float32_mat_of_sexp = Conv.float32_mat_of_sexp
  let sexp_of_float32_mat = Conv.sexp_of_float32_mat
  let compare_float32_mat = Poly.compare
  let quickcheck_generator_float32_mat = Base_quickcheck.Generator.float32_mat
  let quickcheck_shrinker_float32_mat = Base_quickcheck.Shrinker.float32_mat
  let quickcheck_observer_float32_mat = Base_quickcheck.Observer.float32_mat

  let%expect_test "[float32_mat]" =
    test
      (module struct
        type t = float32_mat [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Cons Integer (Cons Integer (Many Float)))) |}]
  ;;

  type vec = Conv.vec

  let vec_sexp_grammar = Conv.vec_sexp_grammar
  let vec_of_sexp = Conv.vec_of_sexp
  let sexp_of_vec = Conv.sexp_of_vec
  let compare_vec = Poly.compare
  let quickcheck_generator_vec = Base_quickcheck.Generator.float64_vec
  let quickcheck_shrinker_vec = Base_quickcheck.Shrinker.float64_vec
  let quickcheck_observer_vec = Base_quickcheck.Observer.float64_vec

  let%expect_test "[vec]" =
    test
      (module struct
        type t = vec [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Many Float)) |}]
  ;;

  type float64_vec = Conv.float64_vec

  let float64_vec_sexp_grammar = Conv.float64_vec_sexp_grammar
  let float64_vec_of_sexp = Conv.float64_vec_of_sexp
  let sexp_of_float64_vec = Conv.sexp_of_float64_vec
  let compare_float64_vec = Poly.compare
  let quickcheck_generator_float64_vec = Base_quickcheck.Generator.float64_vec
  let quickcheck_shrinker_float64_vec = Base_quickcheck.Shrinker.float64_vec
  let quickcheck_observer_float64_vec = Base_quickcheck.Observer.float64_vec

  let%expect_test "[float64_vec]" =
    test
      (module struct
        type t = float64_vec [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Many Float)) |}]
  ;;

  type float32_vec = Conv.float32_vec

  let float32_vec_sexp_grammar = Conv.float32_vec_sexp_grammar
  let float32_vec_of_sexp = Conv.float32_vec_of_sexp
  let sexp_of_float32_vec = Conv.sexp_of_float32_vec
  let compare_float32_vec = Poly.compare
  let quickcheck_generator_float32_vec = Base_quickcheck.Generator.float32_vec
  let quickcheck_shrinker_float32_vec = Base_quickcheck.Shrinker.float32_vec
  let quickcheck_observer_float32_vec = Base_quickcheck.Observer.float32_vec

  let%expect_test "[float32_vec]" =
    test
      (module struct
        type t = float32_vec [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| (List (Many Float)) |}]
  ;;

  type bigstring = Conv.bigstring

  let bigstring_sexp_grammar = Conv.bigstring_sexp_grammar
  let bigstring_of_sexp = Conv.bigstring_of_sexp
  let sexp_of_bigstring = Conv.sexp_of_bigstring
  let compare_bigstring = Poly.compare
  let quickcheck_generator_bigstring = Base_quickcheck.Generator.bigstring
  let quickcheck_shrinker_bigstring = Base_quickcheck.Shrinker.bigstring
  let quickcheck_observer_bigstring = Base_quickcheck.Observer.bigstring

  let%expect_test "[bigstring]" =
    test
      (module struct
        type t = bigstring [@@deriving compare, quickcheck, sexp, sexp_grammar]
      end);
    [%expect {| String |}]
  ;;

  module Exn_converter = Conv.Exn_converter

  let of_string__of__of_sexp = Conv.of_string__of__of_sexp
  let string_of__of__sexp_of = Conv.string_of__of__sexp_of
end

module type S = module type of struct
  include Sexplib.Conv
end

module Completeness (_ : S) : sig end = struct end
include Completeness (Test)
