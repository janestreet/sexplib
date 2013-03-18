(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    pflag ["ocaml"; "compile"] "I" (fun x -> S [A "-I"; A x]);
    flag ["ocamldep"; "ocaml"; "use_pa_sexp_conv"]
      (S [A "-ppopt"; P "syntax/pa_sexp_conv.cma"]);
    flag ["compile"; "ocaml"; "use_pa_sexp_conv"]
      (S [A "-ppopt"; P "syntax/pa_sexp_conv.cma"])
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
