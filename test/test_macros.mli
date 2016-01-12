open Sexplib
open Sexplib.Conv

module type Load = sig
  val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a
  val load_sexps_conv    : string -> (Sexp.t -> 'a) -> 'a Sexp.Annotated.conv list
end

module Make (Load : Load) : sig end

