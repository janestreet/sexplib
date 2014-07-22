(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    pflag ["ocaml"; "compile"] "I" (fun x -> S [A "-I"; A x]);
    flag ["ocamldep"; "ocaml"; "use_pa_sexp_conv"]
      (S [A "-ppopt"; P "syntax/pa_sexp_conv.cma"]);
    flag ["compile"; "ocaml"; "use_pa_sexp_conv"]
      (S [A "-ppopt"; P "syntax/pa_sexp_conv.cma"]);

    let env = BaseEnvLight.load () in
    let ver = BaseEnvLight.var_get "ocaml_version" env in
    let ver = Scanf.sscanf ver "%d.%d" (fun major minor -> (major, minor)) in
    if ver >= (4, 02) then begin
      flag ["ocaml"; "compile"; "use_macro"] & S[A"-ppopt"; A "-DOCAML_4_02"];
      flag ["ocaml"; "ocamldep"; "use_macro"] & S[A"-ppopt"; A "-DOCAML_4_02"];
    end
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
