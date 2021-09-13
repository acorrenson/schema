open Schema
open Combinators
open Schemas

let num = 
  (fun x -> implode x |> int_of_string) <$> (
    many1 (check (function '0'..'9' -> true | _ -> false))
  )

let parser =
  let name = p_ident in
  let body = p_ident in
  let arg = num in
  [%schema "let $name be $arg in $body"]

let parse_op =
  let ident = p_ident in
  let op = token "sum" <|> token "product" in
  let arg = num in
  [%load_schemas "operation"]

let test str =
  let ((arg1, arg2, id, op), _) = Option.get (parse_op (explode str))
  in
  Printf.printf "%s =>\narg1 = %d, arg2 = %d, id = %s, op = %s\n\n" str arg1 arg2 id op

let () =
  List.iter test [
    "let x be the sum of 1 and 2";
    "let y be the sum of 12 and 567";
    "the sum of 0 and 1 results in y";
    "myvar is the product of 3 and 12"
  ]