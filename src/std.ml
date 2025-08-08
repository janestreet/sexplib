module Hashtbl = struct
  include Hashtbl

  let sexp_of_t = Conv.sexp_of_hashtbl
  let t_of_sexp = Conv.hashtbl_of_sexp
end

module Lazy = struct
  include Lazy

  let t_of_sexp = Conv.lazy_t_of_sexp
  let sexp_of_t = Conv.sexp_of_lazy_t
  let sexp_of_t__stack = Conv.sexp_of_lazy_t__stack
  let t_sexp_grammar = Conv.lazy_t_sexp_grammar
end

let sexp_of_unit = Conv.sexp_of_unit
let sexp_of_unit__stack = Conv.sexp_of_unit__stack
let unit_of_sexp = Conv.unit_of_sexp
let unit_sexp_grammar = Conv.unit_sexp_grammar
let sexp_of_bool = Conv.sexp_of_bool
let sexp_of_bool__stack = Conv.sexp_of_bool__stack
let bool_of_sexp = Conv.bool_of_sexp
let bool_sexp_grammar = Conv.bool_sexp_grammar
let sexp_of_string = Conv.sexp_of_string
let sexp_of_string__stack = Conv.sexp_of_string__stack
let string_of_sexp = Conv.string_of_sexp
let string_sexp_grammar = Conv.string_sexp_grammar
let sexp_of_char = Conv.sexp_of_char
let sexp_of_char__stack = Conv.sexp_of_char__stack
let char_of_sexp = Conv.char_of_sexp
let char_sexp_grammar = Conv.char_sexp_grammar
let sexp_of_int = Conv.sexp_of_int
let sexp_of_int__stack = Conv.sexp_of_int__stack
let int_of_sexp = Conv.int_of_sexp
let int_sexp_grammar = Conv.int_sexp_grammar
let sexp_of_float = Conv.sexp_of_float
let sexp_of_float__stack = Conv.sexp_of_float__stack
let float_of_sexp = Conv.float_of_sexp
let float_sexp_grammar = Conv.float_sexp_grammar
let sexp_of_int32 = Conv.sexp_of_int32
let sexp_of_int32__stack = Conv.sexp_of_int32__stack
let int32_of_sexp = Conv.int32_of_sexp
let int32_sexp_grammar = Conv.int32_sexp_grammar
let sexp_of_int64 = Conv.sexp_of_int64
let sexp_of_int64__stack = Conv.sexp_of_int64__stack
let int64_of_sexp = Conv.int64_of_sexp
let int64_sexp_grammar = Conv.int64_sexp_grammar
let sexp_of_nativeint = Conv.sexp_of_nativeint
let sexp_of_nativeint__stack = Conv.sexp_of_nativeint__stack
let nativeint_of_sexp = Conv.nativeint_of_sexp
let nativeint_sexp_grammar = Conv.nativeint_sexp_grammar
let sexp_of_ref = Conv.sexp_of_ref
let sexp_of_ref__stack = Conv.sexp_of_ref__stack
let ref_of_sexp = Conv.ref_of_sexp
let ref_sexp_grammar = Conv.ref_sexp_grammar
let sexp_of_lazy_t = Conv.sexp_of_lazy_t
let sexp_of_lazy_t__stack = Conv.sexp_of_lazy_t__stack
let lazy_t_of_sexp = Conv.lazy_t_of_sexp
let lazy_t_sexp_grammar = Conv.lazy_t_sexp_grammar
let sexp_of_option = Conv.sexp_of_option
let sexp_of_option__stack = Conv.sexp_of_option__stack
let option_of_sexp = Conv.option_of_sexp
let option_sexp_grammar = Conv.option_sexp_grammar
let sexp_of_list = Conv.sexp_of_list
let sexp_of_list__stack = Conv.sexp_of_list__stack
let list_of_sexp = Conv.list_of_sexp
let list_sexp_grammar = Conv.list_sexp_grammar
let sexp_of_array = Conv.sexp_of_array
let sexp_of_array__stack = Conv.sexp_of_array__stack
let array_of_sexp = Conv.array_of_sexp
let array_sexp_grammar = Conv.array_sexp_grammar
let sexp_of_exn = Conv.sexp_of_exn
let exn_sexp_grammar = Conv.sexp_t_sexp_grammar
