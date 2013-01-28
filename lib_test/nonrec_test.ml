open Sexplib
open Conv

type t = float with sexp

module M : sig
  type t = float list with sexp
end = struct
  type nonrec t = t list with sexp
end

type 'a u = 'a with sexp
module M2 : sig
  type 'a u = 'a list with sexp
end = struct
  type nonrec 'a u = 'a u list with sexp
end

type 'a v = 'a w
and 'a w = A of 'a v with sexp
type 'a v_ = 'a v with sexp
type 'a w_ = 'a w with sexp
module M3 : sig
  type 'a v = 'a w_ with sexp
  type 'a w = 'a v_ with sexp
end = struct
  type nonrec 'a v = 'a w
  and 'a w = 'a v with sexp
end
