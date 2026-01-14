(** This file is only kept for compatibility with pa_sexp_conv. *)
val register : exn -> string -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register1 : ('a -> exn) -> string -> ('a -> Sexp.t) @ portable -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register2
  :  ('a -> 'b -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register3
  :  ('a -> 'b -> 'c -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register4
  :  ('a -> 'b -> 'c -> 'd -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register5
  :  ('a -> 'b -> 'c -> 'd -> 'e -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register6
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> ('f -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register7
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> ('f -> Sexp.t) @ portable
  -> ('g -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register8
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> ('f -> Sexp.t) @ portable
  -> ('g -> Sexp.t) @ portable
  -> ('h -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register9
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> ('f -> Sexp.t) @ portable
  -> ('g -> Sexp.t) @ portable
  -> ('h -> Sexp.t) @ portable
  -> ('i -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]

val register10
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> exn)
  -> string
  -> ('a -> Sexp.t) @ portable
  -> ('b -> Sexp.t) @ portable
  -> ('c -> Sexp.t) @ portable
  -> ('d -> Sexp.t) @ portable
  -> ('e -> Sexp.t) @ portable
  -> ('f -> Sexp.t) @ portable
  -> ('g -> Sexp.t) @ portable
  -> ('h -> Sexp.t) @ portable
  -> ('i -> Sexp.t) @ portable
  -> ('j -> Sexp.t) @ portable
  -> unit
[@@deprecated "[2016-07] use Conv.Exn_converter.add"]
