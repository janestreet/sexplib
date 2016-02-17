(* Utility Module for S-expression Conversions *)

open Printf
open Bigarray
open Sexp

module String = Bytes

type sexp_bool = bool
type 'a sexp_option = 'a option
type 'a sexp_list = 'a list
type 'a sexp_array = 'a array
type 'a sexp_opaque = 'a

type bigstring = Sexp.bigstring
type float32_vec = (float, float32_elt, fortran_layout) Array1.t
type float64_vec = (float, float64_elt, fortran_layout) Array1.t
type vec = float64_vec
type float32_mat = (float, float32_elt, fortran_layout) Array2.t
type float64_mat = (float, float64_elt, fortran_layout) Array2.t
type mat = float64_mat

(* Conversion of OCaml-values to S-expressions *)

external format_float : string -> float -> string = "caml_format_float"

(* '%.17g' is guaranteed to be round-trippable.

   '%.15g' will be round-trippable and not have noise at the last digit or two for a float
   which was converted from a decimal (string) with <= 15 significant digits.  So it's
   worth trying first to avoid things like "3.1400000000000001".

   See comment above [to_string_round_trippable] in {!Core_kernel.Float} for
   detailed explanation and examples. *)
let default_string_of_float =
  ref (fun x ->
    let y = format_float "%.15G" x in
    if float_of_string y = x then
      y
    else
      format_float "%.17G" x)
;;

let read_old_option_format = ref true
let write_old_option_format = ref true

let list_map f l = List.rev (List.rev_map f l)

let sexp_of_unit () = List []
let sexp_of_bool b = Atom (string_of_bool b)
let sexp_of_string str = Atom str
let sexp_of_char c = Atom (String.make 1 c)
let sexp_of_int n = Atom (string_of_int n)
let sexp_of_float n = Atom (!default_string_of_float n)
let sexp_of_int32 n = Atom (Int32.to_string n)
let sexp_of_int64 n = Atom (Int64.to_string n)
let sexp_of_nativeint n = Atom (Nativeint.to_string n)
let sexp_of_ref sexp_of__a rf = sexp_of__a !rf
let sexp_of_lazy_t sexp_of__a lv = sexp_of__a (Lazy.force lv)

let sexp_of_option sexp_of__a = function
  | Some x when !write_old_option_format -> List [sexp_of__a x]
  | Some x -> List [Atom "some"; sexp_of__a x]
  | None when !write_old_option_format -> List []
  | None -> Atom "none"

let sexp_of_pair sexp_of__a sexp_of__b (a, b) =
  List [sexp_of__a a; sexp_of__b b]

let sexp_of_triple sexp_of__a sexp_of__b sexp_of__c (a, b, c) =
  List [sexp_of__a a; sexp_of__b b; sexp_of__c c]

(* List.rev (List.rev_map ...) is tail recursive, the OCaml standard
   library List.map is NOT. *)
let sexp_of_list sexp_of__a lst = List (List.rev (List.rev_map sexp_of__a lst))

let sexp_of_array sexp_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.(i) :: !lst_ref
  done;
  List !lst_ref

let sexp_of_hashtbl sexp_of_key sexp_of_val htbl =
  let coll k v acc = List [sexp_of_key k; sexp_of_val v] :: acc in
  List (Hashtbl.fold coll htbl [])

let sexp_of_float_vec vec =
  let lst_ref = ref [] in
  for i = Array1.dim vec downto 1 do
    lst_ref := sexp_of_float vec.{i} :: !lst_ref
  done;
  List !lst_ref

let sexp_of_bigstring (bstr : bigstring) =
  let n = Array1.dim bstr in
  let str = String.create n in
  for i = 0 to n - 1 do str.[i] <- bstr.{i} done;
  Atom str

let sexp_of_float32_vec (vec : float32_vec) = sexp_of_float_vec vec
let sexp_of_float64_vec (vec : float64_vec) = sexp_of_float_vec vec
let sexp_of_vec (vec : vec) = sexp_of_float_vec vec

let sexp_of_float_mat mat =
  let m = Array2.dim1 mat in
  let n = Array2.dim2 mat in
  let lst_ref = ref [] in
  for col = n downto 1 do
    let vec = Array2.slice_right mat col in
    for row = m downto 1 do
      lst_ref := sexp_of_float vec.{row} :: !lst_ref
    done
  done;
  List (sexp_of_int m :: sexp_of_int n :: !lst_ref)

let sexp_of_float32_mat (mat : float32_mat) = sexp_of_float_mat mat
let sexp_of_float64_mat (mat : float64_mat) = sexp_of_float_mat mat
let sexp_of_mat (mat : mat) = sexp_of_float_mat mat

let sexp_of_opaque _ = Atom "<opaque>"
let sexp_of_fun _ = Atom "<fun>"

let string_of__of__sexp_of to_sexp x = Sexp.to_string (to_sexp x)


(* Exception converter registration and lookup *)

module Exn_converter = struct
  type t = int64

  module Ids = Map.Make (Int64)

  let exn_id_cnt = ref Int64.max_int
  let exn_handlers : (exn -> Sexp.t option) Ids.t ref = ref Ids.empty

  (* These exception registration functions assume that context-switches
     cannot happen unless there is an allocation.  It is reasonable to expect
     that this will remain true for the foreseeable future.  That way we
     avoid using mutexes and thus a dependency on the threads library. *)

  let rec add_slow sexp_of_exn =
    let exn_id = !exn_id_cnt in
    let new_exn_id = Int64.sub exn_id Int64.one in
    let new_exn_handlers = Ids.add exn_id sexp_of_exn !exn_handlers in
    (* This trick avoids mutexes and should be fairly efficient *)
    if !exn_id_cnt != exn_id then add_slow sexp_of_exn
    else begin
      (* These two assignments should always be atomic *)
      exn_id_cnt := new_exn_id;
      exn_handlers := new_exn_handlers;
      exn_id
    end

  let rec del_slow exn_id =
    let old_exn_handlers = !exn_handlers in
    let new_exn_handlers = Ids.remove exn_id old_exn_handlers in
    (* This trick avoids mutexes and should be fairly efficient *)
    if !exn_handlers != old_exn_handlers then del_slow exn_id
    else exn_handlers := new_exn_handlers

  exception Found_sexp_opt of Sexp.t option

  let find_slow exn =
    try
      let act _id sexp_of_exn =
        let sexp_opt = sexp_of_exn exn in
        if sexp_opt <> None then raise (Found_sexp_opt sexp_opt)
      in
      Ids.iter act !exn_handlers;
      None
    with Found_sexp_opt sexp_opt -> sexp_opt

  (* Fast and automatic exception registration *)

  module Int = struct
    type t = int

    let compare t1 t2 = compare (t1 : int) t2
  end

  module Exn_ids = Map.Make (Int)

  let exn_id_map : (exn -> Sexp.t) Exn_ids.t ref = ref Exn_ids.empty

  (* [Obj.extension_id] works on both the exception itself, and the extension slot of the
     exception. *)
  let rec clean_up_handler (slot : extension_constructor) =
    let id = Obj.extension_id slot in
    let old_exn_id_map = !exn_id_map in
    let new_exn_id_map = Exn_ids.remove id old_exn_id_map in
    (* This trick avoids mutexes and should be fairly efficient *)
    if !exn_id_map != old_exn_id_map then
      clean_up_handler slot
    else
      exn_id_map := new_exn_id_map

  let add_auto ?(finalise = true) exn sexp_of_exn =
    let id = Obj.extension_id (Obj.extension_constructor exn) in
    let rec loop () =
      let old_exn_id_map = !exn_id_map in
      let new_exn_id_map = Exn_ids.add id sexp_of_exn old_exn_id_map in
      (* This trick avoids mutexes and should be fairly efficient *)
      if !exn_id_map != old_exn_id_map then
        loop ()
      else begin
        exn_id_map := new_exn_id_map;
        if finalise then Gc.finalise clean_up_handler (Obj.extension_constructor exn)
      end
    in
    loop ()

  let find_auto exn =
    let id = Obj.extension_id (Obj.extension_constructor exn) in
    match Exn_ids.find id !exn_id_map with
    | exception Not_found -> None
    | sexp_of_exn -> Some (sexp_of_exn exn)


  let max_exn_tags = ref 20

  let set_max_exn_tags n =
    if n < 1 then
      failwith "Sexplib.Conv.Exn_converter.set_max_exn_tags: n < 1"
    else max_exn_tags := n

  let get_max_exn_tags () = !max_exn_tags
end

let sexp_of_exn_opt exn =
  let sexp_opt = Exn_converter.find_auto exn in
  if sexp_opt = None then Exn_converter.find_slow exn
  else sexp_opt

let sexp_of_exn exn =
  match sexp_of_exn_opt exn with
  | None -> List [Atom (Printexc.to_string exn)]
  | Some sexp -> sexp

let exn_to_string e = Sexp.to_string_hum (sexp_of_exn e)

(* {[exception Blah [@@deriving sexp]]} generates a call to the function
   [Exn_converter.add_auto] defined in this file.  So we are guaranted that as soon as we
   mark an exception as sexpable, this module will be linked in and this printer will be
   registered, which is what we want. *)
let () =
  Printexc.register_printer (fun exn ->
    match sexp_of_exn_opt exn with
    | None -> None
    | Some sexp ->
      Some (Sexp.to_string_hum ~indent:2 sexp))

(* Conversion of S-expressions to OCaml-values *)

exception Of_sexp_error = Pre_sexp.Of_sexp_error

let record_check_extra_fields = ref true

let of_sexp_error_exn exc sexp = raise (Of_sexp_error (exc, sexp))

let of_sexp_error what sexp = raise (Of_sexp_error (Failure what, sexp))

let unit_of_sexp sexp = match sexp with
  | List [] -> ()
  | Atom _ | List _ -> of_sexp_error "unit_of_sexp: empty list needed" sexp

let bool_of_sexp sexp = match sexp with
  | Atom ("true" | "True") -> true
  | Atom ("false" | "False") -> false
  | Atom _ -> of_sexp_error "bool_of_sexp: unknown string" sexp
  | List _ -> of_sexp_error "bool_of_sexp: atom needed" sexp

let string_of_sexp sexp = match sexp with
  | Atom str -> str
  | List _ -> of_sexp_error "string_of_sexp: atom needed" sexp

let char_of_sexp sexp = match sexp with
  | Atom str ->
      if String.length str <> 1 then
        of_sexp_error
          "char_of_sexp: atom string must contain one character only" sexp;
      str.[0]
  | List _ -> of_sexp_error "char_of_sexp: atom needed" sexp

let int_of_sexp sexp = match sexp with
  | Atom str ->
      (try int_of_string str
      with exc -> of_sexp_error ("int_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int_of_sexp: atom needed" sexp

let float_of_sexp sexp = match sexp with
  | Atom str ->
      (try float_of_string str
      with exc ->
        of_sexp_error ("float_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "float_of_sexp: atom needed" sexp

let int32_of_sexp sexp = match sexp with
  | Atom str ->
      (try Int32.of_string str
      with exc ->
        of_sexp_error ("int32_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int32_of_sexp: atom needed" sexp

let int64_of_sexp sexp = match sexp with
  | Atom str ->
      (try Int64.of_string str
      with exc ->
        of_sexp_error ("int64_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int64_of_sexp: atom needed" sexp

let nativeint_of_sexp sexp = match sexp with
  | Atom str ->
      (try Nativeint.of_string str
      with exc ->
        of_sexp_error ("nativeint_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "nativeint_of_sexp: atom needed" sexp

let ref_of_sexp a__of_sexp sexp = ref (a__of_sexp sexp)
let lazy_t_of_sexp a__of_sexp sexp = Lazy.from_val (a__of_sexp sexp)

let option_of_sexp a__of_sexp sexp =
  if !read_old_option_format then
    match sexp with
    | List [] | Atom ("none" | "None") -> None
    | List [el] | List [Atom ("some" | "Some"); el] -> Some (a__of_sexp el)
    | List _ ->
        of_sexp_error "option_of_sexp: list must represent optional value" sexp
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
  else
    match sexp with
    | Atom ("none" | "None") -> None
    | List [Atom ("some" | "Some"); el] -> Some (a__of_sexp el)
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
    | List _ -> of_sexp_error "option_of_sexp: list must be (some el)" sexp

let pair_of_sexp a__of_sexp b__of_sexp sexp = match sexp with
  | List [a_sexp; b_sexp] ->
      let a = a__of_sexp a_sexp in
      let b = b__of_sexp b_sexp in
      a, b
  | List _ ->
      of_sexp_error
        "pair_of_sexp: list must contain exactly two elements only" sexp
  | Atom _ -> of_sexp_error "pair_of_sexp: list needed" sexp

let triple_of_sexp a__of_sexp b__of_sexp c__of_sexp sexp = match sexp with
  | List [a_sexp; b_sexp; c_sexp] ->
      let a = a__of_sexp a_sexp in
      let b = b__of_sexp b_sexp in
      let c = c__of_sexp c_sexp in
      a, b, c
  | List _ ->
      of_sexp_error
        "triple_of_sexp: list must contain exactly three elements only" sexp
  | Atom _ -> of_sexp_error "triple_of_sexp: list needed" sexp

let list_of_sexp a__of_sexp sexp = match sexp with
  | List lst ->
      let rev_lst = List.rev_map a__of_sexp lst in
      List.rev rev_lst
  | Atom _ -> of_sexp_error "list_of_sexp: list needed" sexp

let array_of_sexp a__of_sexp sexp = match sexp with
  | List [] -> [||]
  | List (h :: t) ->
      let len = List.length t + 1 in
      let res = Array.make len (a__of_sexp h) in
      let rec loop i = function
        | [] -> res
        | h :: t -> res.(i) <- a__of_sexp h; loop (i + 1) t in
      loop 1 t
  | Atom _ -> of_sexp_error "array_of_sexp: list needed" sexp

let hashtbl_of_sexp key_of_sexp val_of_sexp sexp = match sexp with
  | List lst ->
      let htbl = Hashtbl.create 0 in
      let act = function
        | List [k_sexp; v_sexp] ->
            Hashtbl.add htbl (key_of_sexp k_sexp) (val_of_sexp v_sexp)
        | List _ | Atom _ ->
            of_sexp_error "hashtbl_of_sexp: tuple list needed" sexp
      in
      List.iter act lst;
      htbl
  | Atom _ -> of_sexp_error "hashtbl_of_sexp: list needed" sexp

let bigstring_of_sexp sexp = match sexp with
  | Atom str ->
      let len = String.length str in
      let bstr = Array1.create char c_layout len in
      for i = 0 to len - 1 do bstr.{i} <- str.[i] done;
      bstr
  | List _ -> of_sexp_error "bigstring_of_sexp: atom needed" sexp

let float_vec_of_sexp empty_float_vec create_float_vec sexp = match sexp with
  | List [] -> empty_float_vec
  | List lst ->
      let len = List.length lst in
      let res = create_float_vec len in
      let rec loop i = function
        | [] -> res
        | h :: t -> res.{i} <- float_of_sexp h; loop (i + 1) t in
      loop 1 lst
  | Atom _ -> of_sexp_error "float_vec_of_sexp: list needed" sexp

let create_float32_vec = Array1.create float32 fortran_layout
let create_float64_vec = Array1.create float64 fortran_layout
let empty_float32_vec = create_float32_vec 0
let empty_float64_vec = create_float64_vec 0
let float32_vec_of_sexp = float_vec_of_sexp empty_float32_vec create_float32_vec
let float64_vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec
let vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec

let check_too_much_data sexp data res =
  if data = [] then res
  else of_sexp_error "float_mat_of_sexp: too much data" sexp

let float_mat_of_sexp create_float_mat sexp = match sexp with
  | List (sm :: sn :: data) ->
      let m = int_of_sexp sm in
      let n = int_of_sexp sn in
      let res = create_float_mat m n in
      if m = 0 || n = 0 then check_too_much_data sexp data res
      else
        let rec loop_cols col data =
          let vec = Array2.slice_right res col in
          let rec loop_rows row = function
            | [] -> of_sexp_error "float_mat_of_sexp: not enough data" sexp
            | h :: t ->
                vec.{row} <- float_of_sexp h;
                if row = m then
                  if col = n then check_too_much_data sexp t res
                  else loop_cols (col + 1) t
                else loop_rows (row + 1) t in
          loop_rows 1 data in
        loop_cols 1 data
  | List _ -> of_sexp_error "float_mat_of_sexp: list too short" sexp
  | Atom _ -> of_sexp_error "float_mat_of_sexp: list needed" sexp

let create_float32_mat = Array2.create float32 fortran_layout
let create_float64_mat = Array2.create float64 fortran_layout

let float32_mat_of_sexp = float_mat_of_sexp create_float32_mat
let float64_mat_of_sexp = float_mat_of_sexp create_float64_mat
let mat_of_sexp = float_mat_of_sexp create_float64_mat

let opaque_of_sexp sexp =
  of_sexp_error "opaque_of_sexp: cannot convert opaque values" sexp

let fun_of_sexp sexp =
  of_sexp_error "fun_of_sexp: cannot convert function values" sexp

let of_string__of__of_sexp of_sexp s =
  try
    let sexp = Sexp.of_string s in
    of_sexp sexp
  with e ->
    failwith (sprintf "of_string failed on %s with %s" s (exn_to_string e))

(* Registering default exception printers *)

let get_flc_error name (file, line, chr) =
  Atom (sprintf "%s %s:%d:%d" name file line chr)

let () =
  List.iter
    (fun (exc, handler) -> Exn_converter.add_auto ~finalise:false exc handler)
    [
      (
        Assert_failure ("", 0, 0),
        (function
        | Assert_failure arg -> get_flc_error "Assert_failure" arg
        | _ -> assert false)
      );(
        Exit,
        (function
        | Exit -> Atom "Exit"
        | _ -> assert false)
      );(
        End_of_file,
        (function
        | End_of_file -> Atom "End_of_file"
        | _ -> assert false)
      );(
        Failure "",
        (function
        | Failure arg -> List [Atom "Failure"; Atom arg ]
        | _ -> assert false)
      );(
        Not_found,
        (function
        | Not_found -> Atom "Not_found"
        | _ -> assert false)
      );(
        Invalid_argument "",
        (function
        | Invalid_argument arg -> List [Atom "Invalid_argument"; Atom arg ]
        | _ -> assert false)
      );(
        Match_failure ("", 0, 0),
        (function
        | Match_failure arg -> get_flc_error "Match_failure" arg
        | _ -> assert false)
      );(
        Sys_error "",
        (function
        | Sys_error arg -> List [Atom "Sys_error"; Atom arg ]
        | _ -> assert false)
      );(
        Arg.Help "",
        (function
        | Arg.Help arg -> List [Atom "Arg.Help"; Atom arg ]
        | _ -> assert false)
      );(
        Arg.Bad "",
        (function
        | Arg.Bad arg -> List [Atom "Arg.Bad"; Atom arg ]
        | _ -> assert false)
      );(
        Lazy.Undefined,
        (function
        | Lazy.Undefined -> Atom "Lazy.Undefined"
        | _ -> assert false)
      );(
        Parsing.Parse_error,
        (function
        | Parsing.Parse_error -> Atom "Parsing.Parse_error"
        | _ -> assert false)
      );(
        Queue.Empty,
        (function
        | Queue.Empty -> Atom "Queue.Empty"
        | _ -> assert false)
      );(
        Scanf.Scan_failure "",
        (function
        | Scanf.Scan_failure arg -> List [Atom "Scanf.Scan_failure"; Atom arg ]
        | _ -> assert false)
      );(
        Stack.Empty,
        (function
        | Stack.Empty -> Atom "Stack.Empty"
        | _ -> assert false)
      );(
        Stream.Failure,
        (function
        | Stream.Failure -> Atom "Stream.Failure"
        | _ -> assert false)
      );(
        Stream.Error "",
        (function
        | Stream.Error arg -> List [Atom "Stream.Error"; Atom arg ]
        | _ -> assert false)
      );(
        Sys.Break,
        (function
        | Sys.Break -> Atom "Sys.Break"
        | _ -> assert false)
      );(
        Of_sexp_error (Exit, Atom ""),
        (function
        | Of_sexp_error (exc, sexp) ->
            List [Atom "Sexplib.Conv.Of_sexp_error"; sexp_of_exn exc; sexp]
        | _ -> assert false)
      );(
        Parse_error {
          Pre_sexp.
          location = "";
          err_msg = "";
          parse_state =
            `Sexp {
              Pre_sexp.
              parse_pos = {
                Pre_sexp.Parse_pos.
                text_line = 0;
                text_char = 0;
                global_offset = 0;
                buf_pos = 0;
              };
              pstack = [];
              pbuf = Buffer.create 0;
            };
        },
        (function
        | Parse_error pe ->
            let ppos =
              match pe.parse_state with
              | `Sexp { parse_pos; pstack=_; pbuf=_ } | `Annot { parse_pos; pstack=_; pbuf=_ } -> parse_pos
            in
            List [
              Atom "Sexplib.Sexp.Parse_error";
              List [
                List [Atom "location"; Atom pe.location];
                List [Atom "err_msg"; Atom pe.err_msg];
                List [Atom "text_line"; sexp_of_int ppos.Parse_pos.text_line];
                List [Atom "text_char"; sexp_of_int ppos.Parse_pos.text_char];
                List [
                  Atom "global_offset"; sexp_of_int ppos.Parse_pos.global_offset
                ];
                List [Atom "buf_pos"; sexp_of_int ppos.Parse_pos.buf_pos];
              ]
            ]
        | _ -> assert false)
      );(
        Of_string_conv_exn.E {
          Of_string_conv_exn.
          exc = Exit;
          sexp = Atom "";
          sub_sexp = Atom "";
        },
        (function
        | Of_string_conv_exn.E osce ->
            List [
              Atom "Sexplib.Sexp.Of_string_conv_exn.E";
              List [
                List [Atom "exc"; sexp_of_exn osce.Of_string_conv_exn.exc];
                List [Atom "sexp"; osce.Of_string_conv_exn.sexp];
                List [Atom "sub_sexp"; osce.Of_string_conv_exn.sub_sexp];
              ]
            ]
        | _ -> assert false)
      );(
        Sexp.Annotated.Conv_exn ("", Exit),
        (function
        | Sexp.Annotated.Conv_exn (loc, exn) ->
            List [
              Atom "Sexplib.Sexp.Annotated.Conv_exn";
              Atom loc;
              sexp_of_exn exn;
            ]
        | _ -> assert false)
      );
    ]
