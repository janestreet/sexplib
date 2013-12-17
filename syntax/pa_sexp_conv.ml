(* Pa_sexp_conv: Preprocessing Module for Automated S-expression Conversions *)

open StdLabels
open MoreLabels

open Printf

open Camlp4
open PreCast

open Syntax

module Gen = Pa_type_conv.Gen

(* Utility functions *)

let replace_variables_by_underscores =
  (Ast.map_ctyp (function
    | <:ctyp@loc< '$_$ >> -> <:ctyp@loc< _ >>
    | ctyp -> ctyp))#ctyp

let mk_rev_bindings loc fps =
  let coll (i, bindings, patts, vars) fp =
    let name = "v" ^ string_of_int i in
    let var_expr = <:expr@loc< $lid:name$ >> in
    let expr =
      match fp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ $var_expr$ >>
      | `Match matchings -> <:expr@loc< match $var_expr$ with [ $matchings$ ] >>
    in
    let patt = <:patt@loc< $lid:name$ >> in
    let bindings = <:binding@loc< $patt$ = $expr$ and $bindings$ >> in
    i - 1, bindings, patt :: patts, var_expr :: vars
  in
  let n = List.length fps in
  let _, bindings, patts, expr =
    List.fold_left ~f:coll ~init:(n, Ast.BiNil loc, [], []) fps
  in
  bindings, patts, expr

let mk_full_type loc type_name tps =
  let coll_args tp _param = <:ctyp@loc< $tp$ _ >> in
  List.fold_left ~f:coll_args ~init:<:ctyp@loc< $lid:type_name$ >> tps
;;

let sexp_type_is_recursive type_name tp =
  Gen.type_is_recursive type_name tp
    ~short_circuit:(function
      | <:ctyp< sexp_opaque $_$ >> -> Some false
      | _ -> None)

let mk_bindings loc fps = mk_rev_bindings loc (List.rev fps)

let unroll_cnv_fp loc var = function
  | `Fun fun_expr -> <:expr@loc< $fun_expr$ $var$ >>
  | `Match matchings -> <:expr@loc< match $var$ with [ $matchings$ ] >>

let unroll_fun_matches loc fp1 fp2 =
  match fp1, fp2 with
  | `Fun fun_expr1, `Fun fun_expr2 ->
      <:expr@loc< $fun_expr1$ $fun_expr2$ >>
  | `Fun fun_expr, `Match matching ->
      <:expr@loc< $fun_expr$ (fun [ $matching$ ]) >>
  | _ -> assert false  (* impossible *)

let rec sig_of_tds cnv = function
  | Ast.TyDcl (loc, type_name, tps, rhs, cl) -> cnv loc type_name tps rhs cl
  | Ast.TyAnd (loc, tp1, tp2) ->
      <:sig_item@loc< $sig_of_tds cnv tp1$; $sig_of_tds cnv tp2$ >>
  | _ -> assert false  (* impossible *)

let type_app base_type types =
  List.fold_left types ~init:base_type ~f:(fun acc typ ->
    let loc = Ast.loc_of_ctyp typ in
    <:ctyp@loc< $acc$ $typ$ >>)
;;

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ], or [None]
   if there are no type parameters.

   A quantified type annotation is required for of_sexp and to_sexp functions for
   non-regular types, such as [ type t 'a = [ A of 'a | B of t 'a 'a ] ]. *)
let mk_poly_type make_mono_type loc type_name type_params =
  let type_params = List.map type_params ~f:Gen.drop_variance_annotations in
  match type_params with
  | [] -> None
  | first :: rest ->
    let unquantified_type = make_mono_type <:ctyp@loc< $lid:type_name$ >> type_params in
    let loc = Ast.loc_of_ctyp unquantified_type in
    (* It's confusing that you need an application between the '!' and the '.'. I would
       have expected a list. *)
    Some <:ctyp@loc< ! $type_app first rest$ . $unquantified_type$ >>

(* This transformation is sensible only when the gadt syntax is used to describe regular
   variants or existential types but not when the return type is constrained like in [type
   'a t = Int : int t]. In this last case, the generated code is not going to compile
   because [sexp_of_t] would have type [('a -> Sexp.t) -> int t -> Sexp.t] and there is
   type constraint ['a. ('a -> Sexp.t) -> 'a t -> Sexp.t]. *)
let regular_variant_of_gadt_syntax = function
  | <:ctyp@loc< $uid:cnstr$ : $args$ -> $return_type$ >> ->
    ignore return_type;
    <:ctyp@loc< $uid:cnstr$ of $args$ >>
  | <:ctyp@loc< $uid:cnstr$ : $return_type$ >> ->
    ignore return_type;
    <:ctyp@loc< $uid:cnstr$ >>
  | ctyp -> ctyp

(* Generators for S-expressions *)

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct
  let rec sig_of_td__loop acc = function
    | [] ->
        let loc = Ast.loc_of_ctyp acc in
        <:ctyp@loc< $acc$ -> Sexplib.Sexp.t >>
    | tp :: tps ->
        let tp = Gen.drop_variance_annotations tp in
        let loc = Ast.loc_of_ctyp tp in
        let sexp_of = sig_of_td__loop <:ctyp@loc< $acc$ $tp$ >> tps in
        <:ctyp@loc< ( $tp$ -> Sexplib.Sexp.t ) -> $sexp_of$ >>

  let sig_of_td loc type_name tps _rhs _cl =
    let sexp_of = sig_of_td__loop <:ctyp@loc< $lid:type_name$ >> tps in
    <:sig_item@loc< value $lid: "sexp_of_" ^ type_name$ : $sexp_of$ >>

  let mk_sig _rec tds = <:sig_item< $sig_of_tds sig_of_td tds$ >>

  let () = Pa_type_conv.add_sig_generator ~delayed:true "sexp_of" mk_sig

  let mk_sig_exn _rec = function
    | <:ctyp@loc< $uid:_$ >> | <:ctyp@loc< $uid:_$ of $_$ >> ->
        <:sig_item@loc< >>
    | tp -> Gen.error tp ~fn:"mk_sig_exn" ~msg:"unknown type"

  let () = Pa_type_conv.add_sig_generator ~delayed:true ~is_exn:true "sexp" mk_sig_exn
end


(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct

  let rec is_polymorphic_variant = function
    | <:ctyp< ($ty$ as $_$) >> -> is_polymorphic_variant ty
    | <:ctyp< private $tp$ >> -> is_polymorphic_variant tp
    | <:ctyp< ( $tup:_$ ) >>
    | <:ctyp< $_$ -> $_$ >>
    | <:ctyp< { $_$ } >>
    | <:ctyp< [ $_$ ] >> -> `Surely_not
    | <:ctyp< [< $_$ ] >> | <:ctyp< [> $_$ ] >>
    | <:ctyp< [= $_$ ] >> -> `Definitely
    | <:ctyp< '$_$ >>
    | <:ctyp< $_$ $_$ >>
    | <:ctyp< $id:_$ >>
    | <:ctyp< >> -> `Maybe
    | <:ctyp< $tp1$ == $tp2$ >> ->
        begin match is_polymorphic_variant tp1 with
        | (`Surely_not | `Definitely) as res -> res
        | `Maybe -> is_polymorphic_variant tp2 end
    | tp -> Gen.unknown_type tp "Sig_generate_of_sexp.is_polymorphic_variant"

  let rec sig_of_td__loop acc = function
    | [] ->
        let loc = Ast.loc_of_ctyp acc in
        <:ctyp@loc< Sexplib.Sexp.t -> $acc$ >>
    | tp :: tps ->
        let tp = Gen.drop_variance_annotations tp in
        let loc = Ast.loc_of_ctyp tp in
        let of_sexp = sig_of_td__loop <:ctyp@loc< $acc$ $tp$ >> tps in
        <:ctyp@loc< ( Sexplib.Sexp.t -> $tp$ ) -> $of_sexp$ >>

  let sig_of_td with_poly loc type_name tps rhs _cl =
    let of_sexp = sig_of_td__loop <:ctyp@loc< $lid:type_name$ >> tps in
    let of_sexp_item =
      <:sig_item@loc< value $lid: type_name ^ "_of_sexp"$ : $of_sexp$; >>
    in
    match with_poly, is_polymorphic_variant rhs with
    | true, `Surely_not ->
        Gen.error rhs ~fn:"Sig_generate_of_sexp.sig_of_td"
          ~msg:"sexp_poly annotation \
            but type is surely not a polymorphic variant"
    | false, (`Surely_not | `Maybe) -> of_sexp_item
    | (true | false), `Definitely
    | true, `Maybe ->
        <:sig_item@loc<
          $of_sexp_item$;
          value $lid: "__" ^ type_name ^ "_of_sexp__"$ : $of_sexp$;
        >>

  let mk_sig with_poly _rec tds =
    <:sig_item< $sig_of_tds (sig_of_td with_poly) tds$ >>

  let () = Pa_type_conv.add_sig_generator ~delayed:true "of_sexp" (mk_sig false)
  let () = Pa_type_conv.add_sig_generator ~delayed:true "of_sexp_poly" (mk_sig true)
end


(* Generates the signature for type conversion to S-expressions *)
module Sig_generate = struct
  let () =
    Pa_type_conv.add_sig_set
      "sexp"
      ~set:["sexp_of"; "of_sexp"]

  let () =
    Pa_type_conv.add_sig_set
      "sexp_poly"
      ~set:["sexp_of"; "of_sexp_poly"]
end


(* Generator for converters of OCaml-values to S-expressions *)
module Generate_sexp_of = struct
  (* Handling of record defaults *)

  type record_field_handler = [ `keep | `drop_default | `drop_if of Ast.expr ]

  let record_field_handlers
      : (Loc.t, record_field_handler) Hashtbl.t
      = Hashtbl.create 0

  let get_record_field_handler loc =
    try Hashtbl.find record_field_handlers loc
    with Not_found -> `keep

  let check_record_field_handler loc =
    if Hashtbl.mem record_field_handlers loc then
      Loc.raise loc (Failure "sexp record field handler defined twice")

  let () =
    Pa_type_conv.add_record_field_generator "sexp_drop_default" (fun tp ->
      let loc = Ast.loc_of_ctyp tp in
      check_record_field_handler loc;
      Hashtbl.replace record_field_handlers ~key:loc ~data:`drop_default)

  let () =
    Pa_type_conv.add_record_field_generator_with_arg "sexp_drop_if"
      Syntax.expr (fun expr_opt tp ->
        let loc = Ast.loc_of_ctyp tp in
        check_record_field_handler loc;
        let test =
          match expr_opt with
          | Some expr -> expr
          | None -> Loc.raise loc (Failure "could not parse expression")
        in
        Hashtbl.replace record_field_handlers ~key:loc ~data:(`drop_if test))

  (* Make abstract calls *)
  let mk_abst_call loc tn rev_path =
    <:expr@loc<
      $id:Gen.ident_of_rev_path loc (("sexp_of_" ^ tn) :: rev_path)$
    >>

  (* Conversion of type paths *)
  let sexp_of_path_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false  (* impossible *)

  (* Conversion of types *)
  let rec sexp_of_type = function
    | <:ctyp@loc< _ >> ->
        `Fun <:expr@loc< fun _ -> Sexp.Atom "_" >>
    | <:ctyp@loc< sexp_opaque $_$ >> ->
        `Fun <:expr@loc< Sexplib.Conv.sexp_of_opaque >>
    | <:ctyp@loc< $tp1$ $tp2$ >> -> `Fun (sexp_of_appl_fun loc tp1 tp2)
    | <:ctyp< ( $tup:tp$ ) >> -> sexp_of_tuple tp
    | <:ctyp@loc< '$parm$ >> -> `Fun <:expr@loc< $lid:"_of_" ^ parm$ >>
    | <:ctyp@loc< $id:id$ >> -> `Fun (sexp_of_path_fun loc id)
    | <:ctyp@loc< $_$ -> $_$ >> ->
        `Fun <:expr@loc< fun _f ->
          Sexplib.Conv.sexp_of_fun Pervasives.ignore >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> -> sexp_of_variant row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> sexp_of_poly parms poly_tp
    | tp -> Gen.unknown_type tp "sexp_of_type"

  (* Conversion of polymorphic types *)
  and sexp_of_appl_fun loc tp1 tp2 =
    match sexp_of_type tp1, sexp_of_type tp2 with
    | `Fun fun_expr1, `Fun fun_expr2 -> <:expr@loc< $fun_expr1$ $fun_expr2$ >>
    | `Fun fun_expr, `Match matching ->
        <:expr@loc< $fun_expr$ (fun [ $matching$ ]) >>
    | _ -> assert false  (* impossible *)


  (* Conversion of tuples *)
  and sexp_of_tuple tp =
    let loc = Ast.loc_of_ctyp tp in
    let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tp []) in
    let bindings, patts, vars = mk_bindings loc fps in
    let in_expr = <:expr@loc< Sexplib.Sexp.List $Gen.mk_expr_lst loc vars$ >> in
    let expr = <:expr@loc< let $bindings$ in $in_expr$ >> in
    `Match <:match_case@loc< ( $tup:Ast.paCom_of_list patts$ ) -> $expr$ >>


  (* Conversion of variant types *)

  and mk_cnv_expr tp =
    let loc = Ast.loc_of_ctyp tp in
    match sexp_of_type tp with
    | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
    | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>

  and sexp_of_variant row_fields =
    let rec loop = function
      | <:ctyp@loc< $tp1$ | $tp2$ >> ->
          <:match_case@loc< $loop tp1$ | $loop tp2$ >>
      | <:ctyp@loc< `$cnstr$ >> ->
          let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
          <:match_case@loc< `$cnstr$ -> Sexplib.Sexp.Atom $str:str$ >>
      | <:ctyp@loc< `$cnstr$ of sexp_list $tp$>> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let cnv_expr =
          match sexp_of_type tp with
          | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
          | `Match matchings ->
              <:expr@loc< fun el -> match el with [ $matchings$ ] >>
        in
        <:match_case@loc<
          `$cnstr$ l ->
             Sexplib.Sexp.List
               [ Sexplib.Sexp.Atom $str:str$ ::
                   Sexplib.Conv.list_map $cnv_expr$ l]
        >>
      | <:ctyp@loc< `$cnstr$ of $tps$ >> ->
          let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
          let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tps []) in
          let bindings, patts, vars = mk_bindings loc fps in
          let cnstr_expr = <:expr@loc< Sexplib.Sexp.Atom $str:str$ >> in
          let expr =
            <:expr@loc<
              let $bindings$ in
              Sexplib.Sexp.List $Gen.mk_expr_lst loc (cnstr_expr :: vars)$
            >>
          in
          <:match_case@loc< `$cnstr$ $Ast.paSem_of_list patts$ -> $expr$ >>
      | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
      | <:ctyp< [= $row_fields$ ] >> -> loop row_fields
      | <:ctyp@loc< $tp1$ $tp2$ >> ->
          let id_path = Gen.get_appl_path loc tp1 in
          let call = sexp_of_appl_fun loc tp1 tp2 in
          <:match_case@loc< #$id_path$ as v -> $call$ v >>
      | <:ctyp@loc< $id:id$ >> | <:ctyp@loc< #$id:id$ >> ->
          let call =
            match Gen.get_rev_id_path id [] with
            | tn :: rev_path -> mk_abst_call loc tn rev_path
            | [] -> assert false  (* impossible *)
          in
          <:match_case@loc< #$id$ as v -> $call$ v >>
      | tp -> Gen.unknown_type tp "sexp_of_variant"
    in
    `Match (loop row_fields)


  (* Polymorphic record fields *)

  and sexp_of_poly parms tp =
    let loc = Ast.loc_of_ctyp tp in
    let bindings =
      let mk_binding parm =
        <:binding@loc< $lid:"_of_" ^ parm$ = Sexplib.Conv.sexp_of_opaque >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match sexp_of_type tp with
    | `Fun fun_expr -> `Fun <:expr@loc< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case@loc<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Conversion of sum types *)

  let rec branch_sum ctyp =
    match regular_variant_of_gadt_syntax ctyp with
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc< $branch_sum tp1$ | $branch_sum tp2$ >>
    | <:ctyp@loc< $uid:cnstr$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        <:match_case@loc< $uid:cnstr$ -> Sexplib.Sexp.Atom $str:str$ >>
    | <:ctyp@loc< $uid:cnstr$ of sexp_list $tp$>> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let cnv_expr =
          match sexp_of_type tp with
          | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
          | `Match matchings ->
              <:expr@loc< fun el -> match el with [ $matchings$ ] >>
        in
        <:match_case@loc<
          $uid:cnstr$ l ->
             Sexplib.Sexp.List
               [Sexplib.Sexp.Atom $str:str$ ::
                   Sexplib.Conv.list_map $cnv_expr$ l]
        >>
    | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let fps = List.map ~f:sexp_of_type (Ast.list_of_ctyp tps []) in
        let cnstr_expr = <:expr@loc< Sexplib.Sexp.Atom $str:str$ >> in
        let bindings, patts, vars = mk_bindings loc fps in
        let patt =
          match patts with
          | [patt] -> patt
          | _ -> <:patt@loc< ( $tup:Ast.paCom_of_list patts$ ) >>
        in
        <:match_case@loc<
          $uid:cnstr$ $patt$ ->
            let $bindings$ in
            Sexplib.Sexp.List $Gen.mk_expr_lst loc (cnstr_expr :: vars)$
        >>
    | tp -> Gen.unknown_type tp "branch_sum"

  let sexp_of_sum alts = `Match (branch_sum alts)


  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p = <:patt@loc< $lid:name$ = $lid:"v_" ^ name$ >> in
    <:patt@loc< $patt$; $p$ >>

  let sexp_of_record_field patt expr name tp ?sexp_of is_empty_expr =
    let loc = Ast.loc_of_ctyp tp in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr =
      match sexp_of_type tp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
      | `Match matchings ->
          <:expr@loc< fun el -> match el with [ $matchings$ ] >>
    in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> <:expr@loc< $sexp_of$ $cnv_expr$ >>
    in
    let expr =
      let v_name = <:expr@loc< $lid: "v_" ^ name$ >> in
      <:expr@loc<
        let bnds =
          if $is_empty_expr loc v_name$ then bnds
          else
            let arg = $cnv_expr$ $v_name$ in
            let bnd =
              Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
            in
            [ bnd :: bnds ]
        in
        $expr$
        >>
    in
    patt, expr

  let sexp_of_default_field patt expr name tp ?sexp_of default =
    sexp_of_record_field patt expr name tp ?sexp_of
      (fun loc expr -> <:expr@loc< Pervasives.(=) $default$ $expr$ >>)

  let sexp_of_record flds_ctyp =
    let flds = Ast.list_of_ctyp flds_ctyp [] in
    let list_empty_expr loc lst = <:expr@loc<
      match $lst$ with
        [ [] -> True
        | _ -> False
        ]
        >>
    in
    let array_empty_expr loc arr = <:expr@loc<
      match $arr$ with
        [ [||] -> True
        | _ -> False
        ]
        >>
    in
    let coll (patt, expr) = function
      | <:ctyp@loc< $lid:name$ : mutable sexp_option $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_option $tp$ >> ->
        let patt = mk_rec_patt loc patt name in
        let vname = <:expr@loc< v >> in
        let cnv_expr = unroll_cnv_fp loc vname (sexp_of_type tp) in
        let expr =
          <:expr@loc<
            let bnds =
              match $lid:"v_" ^ name$ with
                [ None -> bnds
                | Some v ->
                  let arg = $cnv_expr$ in
                  let bnd =
                    Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
                  in
                  [ bnd :: bnds ] ]
            in
            $expr$
            >>
        in
        patt, expr
      | <:ctyp@loc< $lid:name$ : mutable sexp_bool >>
      | <:ctyp@loc< $lid:name$ : sexp_bool >> ->
        let patt = mk_rec_patt loc patt name in
        let expr =
          <:expr@loc<
            let bnds =
              if $lid:"v_" ^ name$ then
                let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$] in
                [ bnd :: bnds ]
              else bnds
            in
            $expr$
            >>
        in
        patt, expr
      | <:ctyp@loc< $lid:name$ : mutable sexp_list $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_list $tp$ >> ->
        sexp_of_record_field patt expr name tp
          ~sexp_of:<:expr@loc< sexp_of_list >> list_empty_expr
      | <:ctyp@loc< $lid:name$ : mutable sexp_array $tp$ >>
      | <:ctyp@loc< $lid:name$ : sexp_array $tp$ >> ->
        sexp_of_record_field patt expr name tp
          ~sexp_of:<:expr@loc< sexp_of_array >> array_empty_expr
      | <:ctyp@loc< $lid:name$ : mutable $tp$ >>
      | <:ctyp@loc< $lid:name$ : $tp$ >> ->
        let opt_default = Pa_type_conv.Gen.find_record_default loc in
        let field_handler = get_record_field_handler loc in
        begin match opt_default, field_handler with
        | None, `drop_default -> Loc.raise loc (Failure "no default to drop")
        | _, `drop_if test ->
          sexp_of_record_field patt expr name tp
              (fun loc expr -> <:expr@loc< $test$ $expr$>>)
        | Some default, `drop_default ->
          sexp_of_default_field patt expr name tp default
        | _, `keep ->
            let patt = mk_rec_patt loc patt name in
            let vname = <:expr@loc< $lid:"v_" ^ name$ >> in
            let cnv_expr = unroll_cnv_fp loc vname (sexp_of_type tp) in
            let expr =
              <:expr@loc<
                let arg = $cnv_expr$ in
                let bnd =
                  Sexplib.Sexp.List [Sexplib.Sexp.Atom $str:name$; arg]
                in
                let bnds = [ bnd :: bnds ] in
                $expr$
              >>
            in
            patt, expr
        end
      | _ -> assert false  (* impossible *)
    in
    let loc = Ast.loc_of_ctyp flds_ctyp in
    let init_expr = <:expr@loc< Sexplib.Sexp.List bnds >> in
    let patt, expr =
      List.fold_left ~f:coll ~init:(<:patt@loc<>>, init_expr) flds
    in
    `Match
      <:match_case@loc<
        { $patt$ } ->
          let bnds = [] in
          $expr$
      >>


  (* Empty type *)
  let sexp_of_nil loc = `Fun <:expr@loc< fun _v -> assert False >>


  (* Generate code from type definitions *)

  let sexp_of_td loc type_name tps rhs =
    let body =
      let rec loop tp =
        Gen.switch_tp_def tp
          ~alias:(fun (_ : Loc.t) tp -> sexp_of_type tp)
          ~sum:(fun (_ : Loc.t) tp -> sexp_of_sum tp)
          ~record:(fun (_ : Loc.t) tp -> sexp_of_record tp)
          ~variants:(fun (_ : Loc.t) tp -> sexp_of_variant tp)
          ~mani:(fun (_ : Loc.t) _tp1 tp2 -> loop tp2)
          ~nil:sexp_of_nil
      in
      match loop rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction and problems with
             recursive types by eta-expanding function definitions *)
          <:expr@loc< fun [ v -> $fun_expr$ v ] >>
      | `Match matchings ->
          <:expr@loc< fun [ $matchings$ ] >>
    in
    let patts =
      List.map tps
        ~f:(fun ty -> <:patt@loc< $lid:"_of_" ^ Gen.get_tparam_id ty$>>)
    in
    let body = Gen.abstract loc patts body in
    let annot =
      match mk_poly_type Sig_generate_sexp_of.sig_of_td__loop loc type_name tps with
      | None -> <:ctyp@loc< $lid:type_name$ -> Sexplib.Sexp.t >>
      | Some typ -> typ
    in
    <:binding@loc< ($lid:"sexp_of_" ^ type_name$ : $annot$) = $body$ >>

  let rec sexp_of_tds = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        sexp_of_td loc type_name tps rhs
    | Ast.TyAnd (loc, tp1, tp2) ->
        <:binding@loc< $sexp_of_tds tp1$ and $sexp_of_tds tp2$ >>
    | _ -> assert false  (* impossible *)

  let sexp_of rec_ tds =
    let binding, recursive, loc =
      match tds with
      | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
          sexp_of_td loc type_name tps rhs,
          rec_ && sexp_type_is_recursive type_name rhs, loc
      | Ast.TyAnd (loc, _, _) as tds -> sexp_of_tds tds, rec_, loc
      | _ -> assert false  (* impossible *)
    in
    if recursive then <:str_item@loc< value rec $binding$ >>
    else <:str_item@loc< value $binding$ >>

  (* Add code generator to the set of known generators *)
  let () = Pa_type_conv.add_generator "sexp_of" sexp_of

  let sexp_of_exn _rec tp =
    let get_full_cnstr str = Pa_type_conv.get_conv_path () ^ "." ^ str in
    let expr =
      match tp with
      | <:ctyp@loc< $uid:cnstr$ >> ->
          let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
          <:expr@loc<
            Sexplib.Exn_magic.register $uid:cnstr$ $str:get_full_cnstr str$
          >>
      | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
          let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
          let ctyps = Ast.list_of_ctyp tps [] in
          let fps = List.map ~f:sexp_of_type ctyps in
          let sexp_converters =
            List.map fps ~f:(function
            | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
            | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>)
          in
          let _, patts, vars = mk_bindings loc fps in
          let register_name = sprintf "register%d" (List.length fps) in
          let make_exc =
            let var_args =
              match vars with
              | [var] -> var
              | _ -> <:expr@loc< $tup:Ast.exCom_of_list vars$ >>
            in
            Gen.abstract loc patts <:expr@loc< $uid:cnstr$ $var_args$ >>
          in
          let call =
            let partial =
              <:expr@loc<
                Sexplib.Exn_magic.$lid:register_name$
                  $make_exc$ $str:get_full_cnstr str$
              >>
            in
            Gen.apply loc partial sexp_converters
          in
          <:expr@loc< $call$ >>
      | tp -> Gen.unknown_type tp "sexp_of_exn"
    in
    let loc = Ast.loc_of_ctyp tp in
    <:str_item@loc< value () = $expr$ >>

  let () = Pa_type_conv.add_generator ~is_exn:true "sexp" sexp_of_exn
end


(* Generator for converters of S-expressions to OCaml-values *)
module Generate_of_sexp = struct
  let mk_abst_call loc tn ?(internal = false) rev_path =
    let tns = tn ^ "_of_sexp" in
    let tns_suff = if internal then "__" ^ tns ^ "__" else tns in
    <:expr@loc< $id:Gen.ident_of_rev_path loc (tns_suff :: rev_path)$ >>

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    <:match_case@loc< Sexplib.Conv_error.No_variant_match _ -> $expr$ >>

  let is_wildcard = function [_] -> true | _ -> false

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc match_last matches =
    if match_last || is_wildcard matches then
      match matches with
      | <:match_case< $_$ -> $expr$ >> :: _ -> expr
      | _ -> assert false  (* impossible *)
    else <:expr@loc< match atom with [ $list:matches$ ] >>

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
      <:match_case@loc<
        $str:str$ -> Sexplib.Conv_error.$lid:call$ _tp_loc _sexp
      >> :: acc
    in
    let exc_no_variant_match =
      <:match_case@loc<
        _ -> Sexplib.Conv_error.no_variant_match _tp_loc _sexp
      >>
    in
    List.fold_left ~f:coll_structs ~init:[exc_no_variant_match] rev_els

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let rec split_row_field (atoms, structs, ainhs, sinhs as acc) = function
    | <:ctyp@loc< `$cnstr$ >> ->
        let tpl = loc, cnstr in
        (
          tpl :: atoms,
          structs,
          `A tpl :: ainhs,
          sinhs
        )
    | <:ctyp@loc< `$cnstr$ of $tps$ >> ->
        (
          atoms,
          (loc, cnstr) :: structs,
          ainhs,
          `S (loc, cnstr, tps) :: sinhs
        )
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        List.fold_left ~f:split_row_field
          ~init:acc (Ast.list_of_ctyp row_fields [])
    | <:ctyp< $_$ $_$ >>
    | (<:ctyp< $id:_$ >> | <:ctyp< #$id:_$ >>) as inh ->
        let iinh = `I inh in
        (
          atoms,
          structs,
          iinh :: ainhs,
          iinh :: sinhs
        )
    | tp -> Gen.unknown_type tp "split_row_field"

  (* Conversion of type paths *)
  let path_of_sexp_fun loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call loc tn rev_path
    | [] -> assert false  (* no empty paths *)

  (* Conversion of types *)
  let rec type_of_sexp = function
    | <:ctyp@loc< sexp_opaque $_$ >> | <:ctyp@loc< _ >> ->
        `Fun <:expr@loc< Sexplib.Conv.opaque_of_sexp >>
    | <:ctyp@loc< sexp_option >> ->
        `Fun <:expr@loc< fun a_of_sexp v -> Some (a_of_sexp v) >>
    | <:ctyp@loc< sexp_list >> ->
        `Fun <:expr@loc< fun a_of_sexp v ->
          Sexplib.Conv.list_of_sexp a_of_sexp v >>
    | <:ctyp@loc< sexp_array >> ->
        `Fun <:expr@loc< fun a_of_sexp v ->
          Sexplib.Conv.array_of_sexp a_of_sexp v >>
    | <:ctyp@loc< $tp1$ $tp2$ >> ->
        let fp1 = type_of_sexp tp1 in
        let fp2 = type_of_sexp tp2 in
        `Fun (unroll_fun_matches loc fp1 fp2)
    | <:ctyp< ( $tup:tp$ ) >> -> tuple_of_sexp tp
    | <:ctyp@loc< '$parm$ >> -> `Fun <:expr@loc< $lid:"_of_" ^ parm$ >>
    | <:ctyp@loc< $id:id$ >> -> `Fun (path_of_sexp_fun loc id)
    | <:ctyp@loc< $_$ -> $_$ >> -> `Fun <:expr@loc< Sexplib.Conv.fun_of_sexp >>
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        variant_of_sexp ?full_type:None row_fields
    | <:ctyp< ! $parms$ . $poly_tp$ >> -> poly_of_sexp parms poly_tp
    | tp -> Gen.unknown_type tp "type_of_sexp"

  (* Conversion of tuples *)
  and tuple_of_sexp tps =
    let fps = List.map ~f:type_of_sexp (Ast.list_of_ctyp tps []) in
    let loc = Ast.loc_of_ctyp tps in
    let bindings, patts, vars = mk_bindings loc fps in
    let n = string_of_int (List.length fps) in
    `Match
      <:match_case@loc<
          Sexplib.Sexp.List $Gen.mk_patt_lst loc patts$ ->
            let $bindings$ in
            ( $tup:Ast.exCom_of_list vars$ )
        | sexp ->
            Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc $int:n$ sexp
      >>

  (* Generate internal call *)
  and mk_internal_call = function
    | <:ctyp@loc< $id:id$ >> | <:ctyp@loc< #$id:id$ >> ->
        let call =
          match Gen.get_rev_id_path id [] with
          | tn :: rev_path -> mk_abst_call loc tn ~internal:true rev_path
          | [] -> assert false  (* impossible *)
        in
        call
    | <:ctyp@loc< $tp1$ $tp2$ >> ->
        let fp1 = `Fun (mk_internal_call tp1) in
        let fp2 = type_of_sexp tp2 in
        unroll_fun_matches loc fp1 fp2
    | _ -> assert false  (* impossible *)

  (* Generate code for matching included variant types *)
  and handle_variant_inh full_type match_last other_matches inh =
    let loc = Ast.loc_of_ctyp inh in
    let fun_expr = mk_internal_call inh in
    let match_exc =
      handle_no_variant_match loc (
        handle_variant_match_last loc match_last other_matches) in
    let new_other_matches =
      [
        <:match_case@loc<
          _ -> try ($fun_expr$ _sexp :> $replace_variables_by_underscores full_type$) with [ $match_exc$ ]
        >>
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
          let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
          let new_match = <:match_case@loc< $str:str$ -> `$cnstr$ >> in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_structs "ptag_takes_args"
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs in
    handle_variant_match_last loc match_last match_atoms_inhs


  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match ~is_variant cnstr tps =
    let loc = Ast.loc_of_ctyp tps in
    let cnstr vars_expr =
      if is_variant then <:expr@loc< `$cnstr$ $vars_expr$ >>
      else <:expr@loc< $uid:cnstr$ $vars_expr$ >>
    in
    match tps with
    | <:ctyp@loc< sexp_list $tp$ >> ->
      let cnv =
        match type_of_sexp tp with
        | `Fun fun_expr -> <:expr@loc< $fun_expr$ >>
        | `Match matchings ->
            <:expr@loc< fun el -> match el with [ $matchings$ ] >>
      in
      cnstr <:expr@loc< Sexplib.Conv.list_map ($cnv$) sexp_args >>
    | _ ->
        let fps = List.map ~f:type_of_sexp (Ast.list_of_ctyp tps []) in
        let bindings, patts, vars = mk_bindings loc fps in
        let good_arg_match =
          let vars_expr =
            match vars with
            | [var_expr] -> var_expr
            | _ -> <:expr@loc< ( $tup:Ast.exCom_of_list vars$ ) >>
          in
          cnstr vars_expr
        in
        let handle_exc =
          if is_variant then "ptag_incorrect_n_args"
          else "stag_incorrect_n_args"
        in
        <:expr@loc<
          match sexp_args with
            [ $Gen.mk_patt_lst loc patts$ -> let $bindings$ in $good_arg_match$
            | _ -> Sexplib.Conv_error.$lid:handle_exc$ _tp_loc _tag _sexp ]
        >>

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tps) ->
          has_structs_ref := true;
          let expr = mk_cnstr_args_match ~is_variant:true cnstr tps in
          let new_match =
            let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
            <:match_case@loc< ($str:str$ as _tag) -> $expr$ >>
          in
          new_match :: other_matches, false
      | `I inh ->
          handle_variant_inh full_type match_last other_matches inh
    in
    let other_matches =
      mk_variant_other_matches loc rev_atoms "ptag_no_args"
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    (
      handle_variant_match_last loc match_last match_structs_inhs,
      !has_structs_ref
    )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag loc full_type row_fields =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:split_row_field ~init:([], [], [], []) row_fields
    in
    let match_struct, has_structs =
      mk_variant_match_struct loc full_type rev_structs_inhs rev_atoms in
    let maybe_sexp_args_patt =
      if has_structs then <:patt@loc< sexp_args >>
      else <:patt@loc< _ >>
    in
    <:match_case@loc<
        Sexplib.Sexp.Atom atom as _sexp ->
          $mk_variant_match_atom loc full_type rev_atoms_inhs rev_structs$
      | Sexplib.Sexp.List
          [Sexplib.Sexp.Atom atom :: $maybe_sexp_args_patt$] as _sexp ->
            $match_struct$
      | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
          Sexplib.Conv_error.nested_list_invalid_poly_var _tp_loc sexp
      | Sexplib.Sexp.List [] as sexp ->
          Sexplib.Conv_error.empty_list_invalid_poly_var _tp_loc sexp
    >>

  (* Generate matching code for variants *)
  and variant_of_sexp ?full_type row_tp =
    let loc = Ast.loc_of_ctyp row_tp in
    let row_fields = Ast.list_of_ctyp row_tp [] in
    let is_contained, full_type =
      match full_type with
      | None -> true, <:ctyp@loc< [= $row_tp$ ] >>
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
      | (<:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >>) as inh :: rest ->
          let rec loop inh row_fields =
            let call =
              <:expr@loc< ( $mk_internal_call inh$ sexp :>
                              $replace_variables_by_underscores full_type$ ) >>
            in
            match row_fields with
            | [] -> call
            | h :: t ->
                let expr =
                  match h with
                  | <:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >> -> loop h t
                  | _ ->
                     let rftag_matches =
                       handle_variant_tag loc full_type row_fields
                     in
                     <:expr@loc< match sexp with [ $rftag_matches$ ] >>
                in
                <:expr@loc<
                  try $call$ with
                  [ $handle_no_variant_match loc expr$ ]
                >>
          in
          <:match_case@loc< sexp -> $loop inh rest$ >>
      | _ :: _ -> handle_variant_tag loc full_type row_fields
      | [] -> assert false  (* impossible *)
    in
    if is_contained then
      `Fun
        <:expr@loc<
          fun sexp ->
            try match sexp with [ $top_match$ ]
            with
            [ Sexplib.Conv_error.No_variant_match (_tp_loc, sexp) ->
                Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
            ]
        >>
    else `Match top_match

  and poly_of_sexp parms tp =
    let loc = Ast.loc_of_ctyp tp in
    let bindings =
      let mk_binding parm =
        <:binding@loc<
          $lid:"_of_" ^ parm$ =
            fun sexp -> Sexplib.Conv_error.record_poly_field_value _tp_loc sexp
        >>
      in
      List.map ~f:mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match type_of_sexp tp with
    | `Fun fun_expr -> `Fun <:expr@loc< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case@loc<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let rec mk_good_sum_matches ctyp =
    match regular_variant_of_gadt_syntax ctyp with
    | <:ctyp@loc< $uid:cnstr$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let lcstr = String.uncapitalize str in
        <:match_case@loc<
          Sexplib.Sexp.Atom ($str:lcstr$ | $str:str$) -> $uid:cnstr$
        >>
    | <:ctyp@loc< $uid:cnstr$ of $tps$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let lcstr = String.uncapitalize str in
        <:match_case@loc<
          (Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lcstr$ | $str:str$ as _tag) ::
              sexp_args] as _sexp) ->
                $mk_cnstr_args_match ~is_variant:false cnstr tps$
        >>
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc<
            $mk_good_sum_matches tp1$
          | $mk_good_sum_matches tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let rec mk_bad_sum_matches ctyp =
    match regular_variant_of_gadt_syntax ctyp with
    | <:ctyp@loc< $uid:cnstr$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let lcstr = String.uncapitalize str in
        <:match_case@loc<
          Sexplib.Sexp.List
            [Sexplib.Sexp.Atom ($str:lcstr$ | $str:str$) :: _] as sexp ->
              Sexplib.Conv_error.stag_no_args _tp_loc sexp
        >>
    | <:ctyp@loc< $uid:cnstr$ of $_$ >> ->
        let str = Pa_type_conv.Gen.regular_constr_of_revised_constr cnstr in
        let lcstr = String.uncapitalize str in
        <:match_case@loc<
          Sexplib.Sexp.Atom ($str:lcstr$ | $str:str$) as sexp ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        >>
    | <:ctyp@loc< $tp1$ | $tp2$ >> ->
        <:match_case@loc<
            $mk_bad_sum_matches tp1$
          | $mk_bad_sum_matches tp2$
        >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for sum types *)
  let sum_of_sexp alts =
    let loc = Ast.loc_of_ctyp alts in
    `Match
      <:match_case@loc<
          $mk_good_sum_matches alts$
        | $mk_bad_sum_matches alts$
        | Sexplib.Sexp.List [Sexplib.Sexp.List _ :: _] as sexp ->
            Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
        | Sexplib.Sexp.List [] as sexp ->
            Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
        | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp
      >>


  (* Record conversions *)

  (* Generate code for extracting record fields *)
  let mk_extract_fields tp =
    let rec loop no_args args = function
      | <:ctyp< $tp1$; $tp2$ >> ->
          let no_args, args = loop no_args args tp2 in
          loop no_args args tp1
      | <:ctyp@loc< $lid:nm$ : mutable sexp_bool >>
      | <:ctyp@loc< $lid:nm$ : sexp_bool>> ->
          let no_args =
            <:match_case@loc<
                $str:nm$ ->
                  if $lid:nm ^ "_field"$.val then
                    duplicates.val := [ field_name :: duplicates.val ]
                  else $lid:nm ^ "_field"$.val := True
              | $no_args$
            >>
          in
          no_args, args
      | <:ctyp@loc< $lid:nm$ : mutable sexp_option $tp$ >>
      | <:ctyp@loc< $lid:nm$ : sexp_option $tp$ >>
      | <:ctyp@loc< $lid:nm$ : mutable $tp$ >>
      | <:ctyp@loc< $lid:nm$ : $tp$ >> ->
          let unrolled =
            unroll_cnv_fp loc <:expr@loc< _field_sexp >> (type_of_sexp tp)
          in
          let args =
            <:match_case@loc<
                $str:nm$ ->
                  match $lid:nm ^ "_field"$.val with
                  [ None ->
                      let fvalue = $unrolled$ in
                      $lid:nm ^ "_field"$.val := Some fvalue
                  | Some _ ->
                      duplicates.val := [ field_name :: duplicates.val ] ]
              | $args$
            >>
          in
          no_args, args
        | _ -> assert false  (* impossible *)
    in
    let handle_extra =
      let loc = Ast.loc_of_ctyp tp in
      <:match_case@loc<
        _ ->
          if Sexplib.Conv.record_check_extra_fields.val then
            extra.val := [ field_name :: extra.val ]
          else ()
      >>
    in
    loop handle_extra handle_extra tp

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result has_poly flds =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop (res_tpls, bi_lst, good_patts as acc) = function
        | <:ctyp@loc< $lid:nm$ : $tp$ >> ->
            let fld = <:expr@loc< $lid:nm ^ "_field"$.val >> in
            let mk_default loc =
              bi_lst, <:patt@loc< $lid:nm ^ "_value"$ >> :: good_patts
            in
            let new_bi_lst, new_good_patts =
              match tp with
              | <:ctyp@loc< sexp_bool >> | <:ctyp@loc< mutable sexp_bool >>
              | <:ctyp@loc< sexp_option $_$ >>
              | <:ctyp@loc< mutable sexp_option $_$ >>
              | <:ctyp@loc< sexp_list $_$ >>
              | <:ctyp@loc< mutable sexp_list $_$ >>
              | <:ctyp@loc< sexp_array $_$ >>
              | <:ctyp@loc< mutable sexp_array $_$ >> -> mk_default loc
              | <:ctyp@loc< $_$ >> ->
                  match Pa_type_conv.Gen.find_record_default loc with
                  | Some _ -> mk_default loc
                  | None ->
                      has_nonopt_fields := true;
                      (
                        <:expr@loc<
                          (Pervasives.(=) $fld$ None, $str:nm$) >> :: bi_lst,
                        <:patt@loc< Some $lid:nm ^ "_value"$ >> :: good_patts
                      )
            in
            (
              <:expr@loc< $fld$ >> :: res_tpls,
              new_bi_lst,
              new_good_patts
            )
        | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
        | _ -> assert false  (* impossible *)
      in
      loop ([], [], []) flds
    in
    let loc = Ast.loc_of_ctyp flds in
    let match_good_expr =
      if has_poly then
        let rec loop acc = function
          | <:ctyp< $tp1$; $tp2$ >> -> loop (loop acc tp2) tp1
          | <:ctyp@loc< $lid:nm$ : $_$ >> ->
              <:expr@loc< $lid:nm ^ "_value"$ >> :: acc
          | _ -> assert false  (* impossible *)
        in
        match loop [] flds with
        | [match_good_expr] -> match_good_expr
        | match_good_exprs ->
            <:expr@loc< $tup:Ast.exCom_of_list match_good_exprs$ >>
      else
        let rec loop = function
          | <:ctyp@loc< $tp1$; $tp2$ >> ->
              <:rec_binding@loc< $loop tp1$; $loop tp2$ >>
          | <:ctyp@loc< $lid:nm$ : mutable sexp_list $_$ >>
          | <:ctyp@loc< $lid:nm$ : sexp_list $_$ >> ->
              <:rec_binding@loc<
                $lid:nm$ =
                  match $lid:nm ^ "_value"$ with
                  [ None -> [] | Some v -> v ]
              >>
          | <:ctyp@loc< $lid:nm$ : mutable sexp_array $_$ >>
          | <:ctyp@loc< $lid:nm$ : sexp_array $_$ >> ->
              <:rec_binding@loc<
                $lid:nm$ =
                  match $lid:nm ^ "_value"$ with
                  [ None -> [||] | Some v -> v ]
              >>
          | <:ctyp@loc< $lid:nm$ : mutable $_$ >>
          | <:ctyp@loc< $lid:nm$ : $_$ >> ->
              begin match Pa_type_conv.Gen.find_record_default loc with
              | None -> <:rec_binding@loc< $lid:nm$ = $lid:nm ^ "_value"$ >>
              | Some default ->
                  <:rec_binding@loc<
                    $lid:nm$ =
                      match $lid:nm ^ "_value"$ with
                      [ None -> $default$ | Some v -> v ]
                  >>
              end
          | _ -> assert false  (* impossible *)
        in
        <:expr@loc< { $loop flds$ } >>
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [res_expr], [res_patt] -> res_expr, res_patt
      | _ ->
          <:expr@loc< $tup:Ast.exCom_of_list res_tpls$ >>,
          <:patt@loc< $tup:Ast.paCom_of_list good_patts$ >>
    in
    if !has_nonopt_fields then
      <:expr@loc<
        match $expr$ with
        [ $patt$ -> $match_good_expr$
        | _ ->
            Sexplib.Conv_error.record_undefined_elements _tp_loc sexp
              $Gen.mk_expr_lst loc bi_lst$
        ]
      >>
    else <:expr@loc< match $expr$ with [ $patt$ -> $match_good_expr$ ] >>

  (* Generate code for converting record fields *)
  let mk_cnv_fields has_poly flds =
    let field_refs =
      let rec loop = function
        | <:ctyp@loc< $tp1$; $tp2$ >> ->
            <:binding@loc< $loop tp1$ and $loop tp2$ >>
        | <:ctyp@loc< $lid:nm$ : sexp_bool >> ->
            <:binding@loc< $lid:nm ^ "_field"$ = ref False >>
        | <:ctyp@loc< $lid:nm$ : $_$ >> ->
            <:binding@loc< $lid:nm ^ "_field"$ = ref None >>
        | _ -> assert false  (* impossible *)
      in
      loop flds
    in
    let mc_no_args_fields, mc_fields_with_args = mk_extract_fields flds in
    let loc = Ast.loc_of_ctyp flds in
    <:expr@loc<
      let $field_refs$ and duplicates = ref [] and extra = ref [] in
      let rec iter = fun
        [ [
            Sexplib.Sexp.List
              [(Sexplib.Sexp.Atom field_name); _field_sexp] ::
            tail
          ] ->
            do {
              match field_name with
              [ $mc_fields_with_args$ ];
              iter tail }
        | [Sexplib.Sexp.List [(Sexplib.Sexp.Atom field_name)] :: tail] ->
            do {
              match field_name with
              [ $mc_no_args_fields$ ];
              iter tail }
        | [((Sexplib.Sexp.Atom _ | Sexplib.Sexp.List _) as sexp) :: _] ->
            Sexplib.Conv_error.record_only_pairs_expected _tp_loc sexp
        | [] -> () ]
      in
      do {
        iter field_sexps;
        if Pervasives.(<>) duplicates.val [] then
          Sexplib.Conv_error.record_duplicate_fields
            _tp_loc duplicates.val sexp
        else if Pervasives.(<>) extra.val [] then
          Sexplib.Conv_error.record_extra_fields _tp_loc extra.val sexp
        else $mk_handle_record_match_result has_poly flds$
      }
    >>

  let rec is_poly = function
    | <:ctyp< $_$ : ! $_$ . $_$ >> -> true
    | <:ctyp< $flds1$; $flds2$ >> -> is_poly flds1 || is_poly flds2
    | _ -> false

  (* Generate matching code for records *)
  let record_of_sexp flds =
    let loc = Ast.loc_of_ctyp flds in
    let handle_fields =
      let has_poly = is_poly flds in
      let cnv_fields = mk_cnv_fields has_poly flds in
      if has_poly then
        let is_singleton_ref = ref true in
        let patt =
          let rec loop = function
            | <:ctyp@loc< $tp1$; $tp2$ >> ->
                is_singleton_ref := false;
                <:patt@loc< $loop tp1$, $loop tp2$ >>
            | <:ctyp@loc< $lid:nm$ : $_$ >> -> <:patt@loc< $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          let patt = loop flds in
          if !is_singleton_ref then patt
          else <:patt@loc< $tup:patt$ >>
        in
        let record_def =
          let rec loop = function
            | <:ctyp@loc< $tp1$; $tp2$ >> ->
                <:rec_binding@loc< $loop tp1$; $loop tp2$ >>
            | <:ctyp@loc< $lid:nm$ : $_$ >> ->
                <:rec_binding@loc< $lid:nm$ = $lid:nm$ >>
            | _ -> assert false  (* impossible *)
          in
          loop flds
        in
        <:expr@loc<
          let $patt$ = $cnv_fields$ in
          { $record_def$ }
        >>
      else cnv_fields
    in
    `Match
      <:match_case@loc<
          Sexplib.Sexp.List field_sexps as sexp -> $handle_fields$
        | Sexplib.Sexp.Atom _ as sexp ->
            Sexplib.Conv_error.record_list_instead_atom _tp_loc sexp
      >>


  (* Empty type *)
  let nil_of_sexp loc =
    `Fun <:expr@loc< fun sexp -> Sexplib.Conv_error.empty_type _tp_loc sexp >>


  (* Generate code from type definitions *)

  let td_of_sexp loc type_name tps rhs =
    let alias_ref = ref `Not_an_alias in
    let handle_alias tp =
      alias_ref :=
        (match tp with
        | <:ctyp< '$_$ >> -> `Alias `Type_var
        | _ -> `Alias `Type_constructor);
      type_of_sexp tp
    in
    let full_type = mk_full_type loc type_name tps in
    let is_variant_ref = ref false in
    let handle_variant row_fields =
      is_variant_ref := true;
      variant_of_sexp ~full_type row_fields
    in
    let body =
      let rec loop tp =
        Gen.switch_tp_def tp
          ~alias:(fun (_ : Loc.t) tp -> handle_alias tp)
          ~sum:(fun (_ : Loc.t) tp -> sum_of_sexp tp)
          ~record:(fun (_ : Loc.t) tp -> record_of_sexp tp)
          ~variants:(fun (_ : Loc.t) tp -> handle_variant tp)
          ~mani:(fun (_ : Loc.t) _tp1 tp2 -> loop tp2)
          ~nil:nil_of_sexp
      in
      match loop rhs with
      | `Fun fun_expr ->
          (* Prevent violation of value restriction and problems with
             recursive types by eta-expanding function definitions *)
          <:expr@loc< fun [ t -> $fun_expr$ t ] >>
      | `Match matchings -> <:expr@loc< fun [ $matchings$ ] >>
    in
    let external_name = type_name ^ "_of_sexp" in
    let internal_name = "__" ^ type_name ^ "_of_sexp__" in
    let arg_patts, arg_exprs =
      List.split (
        List.map ~f:(function tp ->
            let name = "_of_" ^ Gen.get_tparam_id tp in
            <:patt@loc< $lid:name$ >>, <:expr@loc< $lid:name$ >>
          )
          tps)
    in
    let with_poly_call =
      match !alias_ref with
      | `Not_an_alias
      | `Alias `Type_constructor -> false
      | `Alias `Type_var -> true in
    let internal_fun_body =
      let full_type_name =
        sprintf "%s.%s" (Pa_type_conv.get_conv_path ()) type_name
      in
      if with_poly_call then
        (* special case for type definitions whose bodies are type variables, like:
             type ('a, 'b) t = 'a
           because
           - they can used in polymorphic variants: [ ([`A], int) t | `B ]
           - the way sexplib works, it cannot handle backtracking in these cases,
             (because we only receive as parameter sexp_of_'a but not
             __sexp_of_'a__ presumably)
             so it is better to emit an error rather than do something weird
        *)
        Gen.abstract loc arg_patts
          <:expr@loc<
            fun sexp ->
              Sexplib.Conv_error.silly_type $str:full_type_name$ sexp
          >>
      else
        <:expr@loc<
          let _tp_loc = $str:full_type_name$ in
          $Gen.abstract loc arg_patts body$
        >>
    in
    let pre_external_fun_body =
      let internal_call =
        let internal_expr = <:expr@loc< $lid:internal_name$ >> in
        <:expr@loc< $Gen.apply loc internal_expr arg_exprs$ sexp >>
      in
      let no_variant_match_mc =
        <:match_case@loc<
          Sexplib.Conv_error.No_variant_match (_tp_loc, sexp) ->
            Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
        >>
      in
      if with_poly_call then
        <:expr@loc< try $body$ sexp with [ $no_variant_match_mc$ ] >>
      (* Type alias may refer to variant, therefore same handling here! *)
      else if !is_variant_ref || !alias_ref = `Alias `Type_constructor then
        <:expr@loc< try $internal_call$ with [ $no_variant_match_mc$ ] >>
      else internal_call
    in
    let external_fun_body =
      Gen.abstract loc arg_patts
        <:expr@loc< fun sexp -> ($pre_external_fun_body$) >>
    in
    let type_of_of_sexp =
      mk_poly_type Sig_generate_of_sexp.sig_of_td__loop loc type_name tps
    in
    let annot =
      match type_of_of_sexp with
      | None -> <:ctyp@loc< Sexplib.Sexp.t -> $lid:type_name$ >>
      | Some typ -> typ
    in
    let internal_binding =
      <:binding@loc< ($lid:internal_name$ : $annot$) = $internal_fun_body$ >> in
    let external_binding =
      <:binding@loc< ($lid:external_name$ : $annot$) = $external_fun_body$ >> in
    internal_binding, external_binding

  let rec tds_of_sexp acc = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        td_of_sexp loc type_name tps rhs :: acc
    | Ast.TyAnd (_, tp1, tp2) -> tds_of_sexp (tds_of_sexp acc tp2) tp1
    | _ -> assert false  (* impossible *)

  (* Generate code from type definitions *)
  let of_sexp rec_ = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        let internal_binding, external_binding =
          td_of_sexp loc type_name tps rhs
        in
        let is_recursive = rec_ && sexp_type_is_recursive type_name rhs in
        if is_recursive then
          <:str_item@loc<
            value rec $internal_binding$
            and $external_binding$;
          >>
        else
          <:str_item@loc<
            value $internal_binding$;
            value $external_binding$;
          >>
    | Ast.TyAnd (loc, _, _) as tds ->
        let two_bindings = tds_of_sexp [] tds in
        let bindings =
          List.map ~f:(fun (b1, b2) -> <:binding@loc< $b1$ and $b2$ >>)
            two_bindings
        in
        if rec_ then
          <:str_item@loc<
            value rec $list:bindings$;
          >>
        else
          <:str_item@loc<
            value $list:bindings$;
          >>
    | _ -> assert false  (* impossible *)

  (* Add code generator to the set of known generators *)
  let () = Pa_type_conv.add_generator "of_sexp" of_sexp
end

module Quotations = struct
  let of_sexp_quote loc _loc_name_opt cnt_str =
    Pa_type_conv.set_conv_path_if_not_set loc;
    let ctyp = Gram.parse_string ctyp_quot loc cnt_str in
    let fp = Generate_of_sexp.type_of_sexp ctyp in
    let body =
      match fp with
      | `Fun fun_expr -> <:expr@loc< $fun_expr$ sexp >>
      | `Match matchings -> <:expr@loc< match sexp with [$matchings$] >>
    in
    let full_type_name =
      sprintf "%s line %i: %s"
        (Pa_type_conv.get_conv_path ()) (Loc.start_line loc) cnt_str
    in
    <:expr@loc<
      fun [ sexp ->
        let _tp_loc = $str:full_type_name$ in
        $body$ ]
      >>

  let () =
    Quotation.add "of_sexp" Quotation.DynAst.expr_tag of_sexp_quote

  let sexp_of_quote loc _loc_name_opt cnt_str =
    Pa_type_conv.set_conv_path_if_not_set loc;
    let ctyp = Gram.parse_string ctyp_quot loc cnt_str in
    Generate_sexp_of.mk_cnv_expr ctyp

  let () = Quotation.add "sexp_of" Quotation.DynAst.expr_tag sexp_of_quote
end

(* Add "of_sexp" and "sexp_of" as "sexp" to the set of generators *)
let () =
  Pa_type_conv.add_str_set
    "sexp"
    ~set:["of_sexp"; "sexp_of"]
