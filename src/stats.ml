open Lexer

let stats (l : token list) =
  let open Hashtbl in
  let counts : (token, int) t = create 10 in
  List.iter (fun x ->
    if mem counts x then
      replace counts x (find counts x + 1)
    else
      add counts x 1
  ) l;
  counts

let pp_stat fmt s =
  Hashtbl.iter (fun x c ->
    Format.fprintf fmt "%s => %d\n" (string_of_token x) c
  ) s