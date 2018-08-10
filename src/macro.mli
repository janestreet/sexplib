(**
   This module implements a limited form of evaluation within s-expressions, usually
   called macro expansion. The follow macros are supported:

   {ul
   {- [(:include filename)] expands to the list of s-expressions contained in [filename],
   as if the contents of [filename] were directly inserted in place of [(:include
   filename)].  A relative [filename] is taken with respect to the file that contains the
   include macro.}

   {- [(:let v (v1 ... vn) S1 ... Sm)] defines a macro-variable [v] with
   labelled parameters [v1, ..., vn] and body [S1 ... Sm]. The definition itself
   expands to nothing.  All macro-variables ([v] and [v1 .. vn] here) are lexically
   scoped. An error is reported if [S1 ... Sm] is empty. .}

   {- [(:use v (v1 SS1) ... (vn SSn))] calls the macro-variable [v] with the expansions of
   [SS1 .. SSn], i.e. expands to the body of [v] and substitutes [SS1 .. SSn] for the
   parameters [v1, ..., vn] of [v]. Parameters take no arguments. An error is reported if
   [v] is not in scope, or the parameter labels do not match. }

   {- [(:concat S1 ... Sn)] expands [S1 ... Sn], and assuming the expansions are all
   atoms, expands to a single atom of their concatenation. An error is reported
   otherwise.}
   }

   Macros other than [:include] will be called 'local'. All [:include] macros are expanded
   before all local macros, so [(:include (:use x))] is always an error and if file1
   includes file2 and file2 include file3, file1 can use templates defined in file3.
   However files can only use macro-variables that they brought in scope: if file1 defines
   a macro-variable [a] and includes [file2], file2 cannot use [a].

   The [load...] functions of this module mirror the corresponding functions of
   the [Sexp] module except that they do macro-expansion in the loaded file and
   may throw additional exceptions.

   Example
   -------

   Assume that [input.sexp] contains
   {v
   (:include defs.sexp)
   (:include template.sexp)
   (:use f (a (:use a)) (b (:use b)))
   v}

   the file [defs.sexp] contains
   {v
   (:let a () hello)
   (:let b () " world")
   v}

   and the file [template.sexp] contains
   {v
   (:let f (a b) (:concat (:use a) (:use b)))
   v}

   Then [load_sexp "input.sexp"] will return "hello world".
*)

type 'a conv =
  [ `Result of 'a | `Error of exn * Sexp.t ]

val sexp_of_conv : ('a -> Sexp.t) -> 'a conv -> Sexp.t

type 'a annot_conv = ([ `Result of 'a | `Error of exn * Sexp.Annotated.t ] as 'body)
  constraint 'body = 'a Sexp.Annotated.conv

val sexp_of_annot_conv : ('a -> Sexp.t) -> 'a annot_conv -> Sexp.t

val load_sexp : string -> Sexp.t
(** [load_sexp file] like [{!Sexp.load_sexp} file], but resolves the macros
    contained in [file]. *)

val load_sexps : string -> Sexp.t list
(** [load_sexps file] like [{!Sexp.load_sexps} file], but resolves the macros
    contained in [file]. *)

val load_sexp_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv
(** [load_sexp_conv file f] uses {!load_sexp} and converts the result using
    [f]. *)

val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv list
(** [load_sexps_conv file f] uses {!load_sexps} and converts the result using
    [f]. *)

val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a
(** [load_sexp_conv_exn file f] like {!load_sexp_conv}, but raises an exception
    in case of conversion error. *)

val load_sexps_conv_exn : string -> (Sexp.t -> 'a) -> 'a list
(** [load_sexps_conv_exn file f] like {!load_sexps_conv}, but raises an
    exception in case of conversion error. *)

val expand_local_macros : Sexp.t list -> Sexp.t list conv
(** [expand_local_macros sexps] takes a list of sexps and performs macro-expansion on
    them, except that an error will be returned if an :include macro is found. *)

(** A version of [load_sexps] that is functorized with respect to the functions
    that load the sexps from files and the corresponding monad. *)
module type Sexp_loader = sig
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    module Monad_infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    end
    module List : sig
      val iter : 'a list -> f:('a -> unit t) -> unit t
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end
  val load_sexps           : string -> Sexp.t           list Monad.t
  val load_annotated_sexps : string -> Sexp.Annotated.t list Monad.t
end

module Loader (S : Sexp_loader) : sig
  val load_sexp_conv  : string -> (Sexp.t -> 'a) -> 'a annot_conv      S.Monad.t
  val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a annot_conv list S.Monad.t
end

val add_error_location : string -> exn -> exn
