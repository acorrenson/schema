open Combinators
open Ppxlib

type token =
  | Word of string
  | Ref of string

type schema = token list list

let p_alpha =
  check (function 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let p_ident =
  implode <$> many1 p_alpha

let p_word = (fun x -> Word x) <$> p_ident

let p_ref = (fun x -> Ref x) <$> check ((=) '$') *> p_ident

let p_spaces = many (check ((=) ' '))

let schema =
  many1 (p_spaces *> (p_word <|> p_ref))

let get_ref (s : token list) =
  let rec step acc = function
    | [] -> acc
    | Ref r::xs -> step (r::acc) xs
    | _::xs -> step acc xs
  in step [] s

(* module M = Ast_builder.Make (struct let loc = 1 end) *)

let compile_one ~loc prev x next =
  let open (Ast_builder.Make (struct let loc = loc end)) in
  [%expr fun [%pat x] ->
    let* rp = token [%e estring prev] in
    let* rx = [%e evar x] in
    let* rn = token [%e estring next] in
    return (rp, rx, rn)
  ]

(* let compile_one (s : token list) env =
  let p_token = function
    | Word s -> token s
    | Ref s -> assert false
  in assert false *)


  

(* let compile (s : schema) = *)
