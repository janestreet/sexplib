#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"sexplib"
  [ oasis_lib "sexplib"
  ; oasis_lib "sexplib_num"
  ; oasis_lib "sexplib_unix"
  ; file "META" ~section:"lib"
  ]
