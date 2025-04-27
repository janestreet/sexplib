open Sexplib

(** [sexp_of_big_int n] converts the value [n] of type [Big_int.big_int] to an
    S-expression. *)
val sexp_of_big_int : Big_int.big_int -> Sexp.t

(** [sexp_of_nat n] converts the value [n] of type [Nat.nat] to an S-expression. *)
val sexp_of_nat : Nat.nat -> Sexp.t

(** [sexp_of_ratio n] converts the value [n] of type [Ratio.ratio] to an S-expression. *)
val sexp_of_ratio : Ratio.ratio -> Sexp.t

(** [sexp_of_num n] converts the value [n] of type [Num.num] to an S-expression. *)
val sexp_of_num : Num.num -> Sexp.t

(** [big_int_of_sexp sexp] converts S-expression [sexp] to a value of type
    [Big_int.big_int]. *)
val big_int_of_sexp : Sexp.t -> Big_int.big_int

(** [nat_of_sexp sexp] converts S-expression [sexp] to a value of type [Nat.nat]. *)
val nat_of_sexp : Sexp.t -> Nat.nat

(** [ratio_of_sexp sexp] converts S-expression [sexp] to a value of type [Nat.ratio]. *)
val ratio_of_sexp : Sexp.t -> Ratio.ratio

(** [num_of_sexp sexp] converts S-expression [sexp] to a value of type [Nat.num]. *)
val num_of_sexp : Sexp.t -> Num.num
