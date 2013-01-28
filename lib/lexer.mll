{
  (** Lexer: Lexer Specification for S-expressions *)

  open Printf
  open Lexing
  open Parser

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c -> c

  let lf = '\010'

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let found_newline ({ lex_curr_p; _ } as lexbuf) diff =
    lexbuf.lex_curr_p <-
      {
        lex_curr_p with
        pos_lnum = lex_curr_p.pos_lnum + 1;
        pos_bol = lex_curr_p.pos_cnum - diff;
      }

  let lexeme_len lexbuf = lexeme_end lexbuf - lexeme_start lexbuf

  let main_failure lexbuf msg =
    let { pos_lnum; pos_bol; pos_cnum; _ } = lexeme_start_p lexbuf in
    let msg =
      sprintf
        "Sexplib.Lexer.main: %s at line %d char %d"
        msg pos_lnum (pos_cnum - pos_bol)
    in
    failwith msg
}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']
let unquoted = [^ ';' '(' ')' '"'] # blank # lf_cr
let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']

let unquoted_start =
  unquoted # ['#' '|'] | '#' unquoted # ['|'] | '|' unquoted # ['#']

rule main buf = parse
  | lf | dos_newline { found_newline lexbuf 0; main buf lexbuf }
  | blank+ | ';' (_ # lf_cr)* { main buf lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '"'
      {
        scan_string buf (lexeme_start_p lexbuf) lexbuf;
        let str = Buffer.contents buf in
        Buffer.clear buf;
        STRING str
      }
  | "#;" { SEXP_COMMENT }
  | "#|"
      {
        scan_block_comment buf [lexeme_start_p lexbuf] lexbuf;
        main buf lexbuf
      }
  | "|#" { main_failure lexbuf "illegal end of comment" }
  | unquoted_start unquoted* ("#|" | "|#") unquoted*
      { main_failure lexbuf "comment tokens in unquoted atom" }
  | "#" | unquoted_start unquoted* as str { STRING str }
  | eof { EOF }

and scan_string buf start = parse
  | '"' { () }
  | '\\' lf [' ' '\t']*
      {
        found_newline lexbuf (lexeme_len lexbuf - 2);
        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        found_newline lexbuf (lexeme_len lexbuf - 3);
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let { pos_lnum; pos_bol; pos_cnum; _ } = lexeme_end_p lexbuf in
          let msg =
            sprintf
              "Sexplib.Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos_lnum (pos_cnum - pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf start lexbuf
      }
  | lf
      {
        found_newline lexbuf 0;
        Buffer.add_char buf lf;
        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexeme_start lexbuf in
        let len = lexeme_end lexbuf - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;
        scan_string buf start lexbuf
      }
  | eof
      {
        let msg =
          sprintf
            "Sexplib.Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      }

and scan_block_comment buf locs = parse
  | ('#'* | '|'*) lf
      { found_newline lexbuf 0; scan_block_comment buf locs lexbuf }
  | (('#'* | '|'*) [^ '"' '#' '|'] # lf)+ { scan_block_comment buf locs lexbuf }
  | ('#'* | '|'*) '"'
      {
        let cur = lexeme_end_p lexbuf in
        let start = { cur with pos_cnum = cur.pos_cnum - 1 } in
        scan_string buf start lexbuf;
        Buffer.clear buf;
        scan_block_comment buf locs lexbuf
      }
  | '#'+ '|'
    {
      let cur = lexeme_end_p lexbuf in
      let start = { cur with pos_cnum = cur.pos_cnum - 2 } in
      scan_block_comment buf (start :: locs) lexbuf
    }
  | '|'+ '#'
      {
        match locs with
        | [_] -> ()
        | _ :: t -> scan_block_comment buf t lexbuf
        | [] -> assert false  (* impossible *)
      }
  | eof
      {
        match locs with
        | [] -> assert false
        | { pos_lnum; pos_bol; pos_cnum; _ } :: _ ->
            let msg =
              sprintf "Sexplib.Lexer.scan_block_comment: \
                unterminated block comment at line %d char %d"
                pos_lnum (pos_cnum - pos_bol)
            in
            failwith msg
      }

{
  let main ?buf =
    let buf =
      match buf with
      | None -> Buffer.create 64
      | Some buf -> Buffer.clear buf; buf
    in
    main buf
}
