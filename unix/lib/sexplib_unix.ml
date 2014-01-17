open Sexplib.Sexp
open Sexplib.Conv

let () =
  Exn_converter.add_auto ~finalise:false (Unix.Unix_error (Unix.E2BIG, "", ""))
    (function
      | Unix.Unix_error (err, loc, arg) ->
        let err_str = Unix.error_message err in
        List [Atom "Unix.Unix_error"; Atom err_str; Atom loc; Atom arg]
      | _ -> assert false)
