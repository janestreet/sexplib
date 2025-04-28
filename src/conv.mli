(** Utility Module for S-expression Conversions *)

open Bigarray
include module type of Sexplib0.Sexp_conv

(** {6 Type aliases} *)

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
type float32_vec = (float, float32_elt, fortran_layout) Array1.t
type float64_vec = (float, float64_elt, fortran_layout) Array1.t
type vec = float64_vec
type float32_mat = (float, float32_elt, fortran_layout) Array2.t
type float64_mat = (float, float64_elt, fortran_layout) Array2.t
type mat = float64_mat

(** {6 Conversion of OCaml-values to S-expressions} *)

(** [sexp_of_bigstring bstr] converts a bigstring (character bigarray in C-layout) to an
    S-expression. *)
val sexp_of_bigstring : bigstring -> Sexp.t

(** [sexp_of_float32_vec vec] converts the one-dimensional bigarray [vec] of 32-bit floats
    in Fortran-layout to an S-expression. *)
val sexp_of_float32_vec : float32_vec -> Sexp.t

(** [sexp_of_float64_vec vec] converts the one-dimensional bigarray [vec] of 64-bit floats
    in Fortran-layout to an S-expression. *)
val sexp_of_float64_vec : float64_vec -> Sexp.t

(** [sexp_of_vec vec] same as {!Conv.sexp_of_float64_vec}. *)
val sexp_of_vec : vec -> Sexp.t

(** [sexp_of_float32_mat mat] converts the two-dimensional bigarray [mat] of 32-bit floats
    in Fortran-layout to an S-expression. *)
val sexp_of_float32_mat : float32_mat -> Sexp.t

(** [sexp_of_float64_mat mat] converts the two-dimensional bigarray [mat] of 64-bit floats
    in Fortran-layout to an S-expression. *)
val sexp_of_float64_mat : float64_mat -> Sexp.t

(** [sexp_of_mat mat] same as {!Conv.sexp_of_float64_mat}. *)
val sexp_of_mat : mat -> Sexp.t

(** [string_of__of__sexp_of conv x] converts the OCaml-value [x] to an S-expression
    represented as a string by using conversion function [conv]. *)
val string_of__of__sexp_of : ('a -> Sexp.t) -> 'a -> string

(** {6 Conversion of S-expressions to OCaml-values} *)

val bigstring_sexp_grammar : bigstring Sexplib0.Sexp_grammar.t

(** [bigstring_of_sexp sexp] converts S-expression [sexp] to a bigstring (character
    bigarray in C-layout). *)
val bigstring_of_sexp : Sexp.t -> bigstring

val float32_vec_sexp_grammar : float32_vec Sexplib0.Sexp_grammar.t

(** [float32_vec_of_sexp sexp] converts S-expression [sexp] to a one-dimensional bigarray
    of 32-bit floats in Fortran-layout. *)
val float32_vec_of_sexp : Sexp.t -> float32_vec

val float64_vec_sexp_grammar : float64_vec Sexplib0.Sexp_grammar.t

(** [float64_vec_of_sexp sexp] converts S-expression [sexp] to a one-dimensional bigarray
    of 64-bit floats in Fortran-layout. *)
val float64_vec_of_sexp : Sexp.t -> float64_vec

val vec_sexp_grammar : vec Sexplib0.Sexp_grammar.t

(** [vec_of_sexp sexp] same as {!float64_vec_of_sexp}. *)
val vec_of_sexp : Sexp.t -> vec

val float32_mat_sexp_grammar : float32_mat Sexplib0.Sexp_grammar.t

(** [float32_mat_of_sexp sexp] converts S-expression [sexp] to a two-dimensional bigarray
    of 32-bit floats in Fortran-layout. *)
val float32_mat_of_sexp : Sexp.t -> float32_mat

val float64_mat_sexp_grammar : float64_mat Sexplib0.Sexp_grammar.t

(** [float64_mat_of_sexp sexp] converts S-expression [sexp] to a two-dimensional bigarray
    of 64-bit floats in Fortran-layout. *)
val float64_mat_of_sexp : Sexp.t -> float64_mat

val mat_sexp_grammar : mat Sexplib0.Sexp_grammar.t

(** [mat_of_sexp sexp] same as {!Conv.float64_mat_of_sexp}. *)
val mat_of_sexp : Sexp.t -> mat

(** [of_string__of__of_sexp conv str] converts the S-expression [str] represented as a
    string to an OCaml-value by using conversion function [conv]. *)
val of_string__of__of_sexp : (Sexp.t -> 'a) -> string -> 'a
