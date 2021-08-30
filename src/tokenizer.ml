open Lexer

let tokenize lexbuf =
  let blocks = ref [] in
  try while true do
      blocks := token lexbuf::!blocks
    done; []
  with End_of_file -> List.rev !blocks

let tokenize_string s =
  tokenize (Lexing.from_string s)

let tokenize_file f =
  let ic = open_in f in
  let res = tokenize (Lexing.from_channel ic) in
  close_in ic;
  res