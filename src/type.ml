(** Type of S-expressions *)
type t = Sexp0.Sexp.t = Atom of string | List of t list
