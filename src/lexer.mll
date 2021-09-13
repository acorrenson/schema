{
  open Lexing
  type token =
    | Dot
    | Com
    | Col
    | SCol
    | LPar
    | RPar
    | Quote
    | SQuote
    | Slash
    | Qmark
    | Bang
    | Word of string
    | Int of int

  let string_of_token = function
    | Dot -> "."
    | Com -> ","
    | Col -> ":"
    | SCol -> ";"
    | LPar -> "("
    | RPar -> ")"
    | Quote -> "\""
    | SQuote -> "\'"
    | Slash -> "/"
    | Qmark -> "?"
    | Bang -> "!"
    | Word w -> w
    | Int i -> string_of_int i

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
        pos_bol = pos.pos_cnum;
        pos_lnum = pos.pos_lnum + 1
      }
  
  let print_position outx pos =
    Printf.fprintf outx "%s:%d:%d\n" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let word = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | '.'     { Dot }
  | ','     { Com }
  | ':'     { Col }
  | ';'     { SCol }
  | '/'     { Slash }
  | '?'     { Qmark }
  | '!'     { Bang }
  | '('     { LPar }
  | ')'     { RPar }
  | '"'     { Quote }
  | '\''    { SQuote }
  | white   { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | int     { Int (int_of_string (lexeme lexbuf)) }
  | word    { Word (lexeme lexbuf) }
  | eof     { raise End_of_file }
  | _       { 
    Printf.eprintf "unknown token %s at (%a)" (lexeme lexbuf)
      print_position lexbuf.lex_curr_p;
    assert false
  }