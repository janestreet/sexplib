val register : exn -> string -> unit

val register1 :
  ('a -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> unit

val register2 :
  ('a -> 'b -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> unit

val register3 :
  ('a -> 'b -> 'c -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> unit

val register4 :
  ('a -> 'b -> 'c -> 'd -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> unit

val register5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> unit

val register6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> unit

val register7 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> unit

val register8 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> unit

val register9 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> ('i -> Sexp.t)
  -> unit

val register10 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> ('i -> Sexp.t)
  -> ('j -> Sexp.t)
  -> unit
