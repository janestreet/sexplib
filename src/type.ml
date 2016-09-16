(** Type of S-expressions *)
type t = Base0.Sexplib.Sexp.t = Atom of string | List of t list
