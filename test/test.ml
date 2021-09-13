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
  let action ~name1 ~arg1 ~body1 () = (name1, arg1, body1) in
  [%schema "let $name be $arg in $body", action]

let () =
  let ((n, a, b), _) = Option.get (parser (explode "let x be 1 in y")) in
  Printf.printf "id = %s, val = %d, body = %s\n" n a b

let parse_op =
  let ident = p_ident in
  let op = token "sum" <|> token "product" in
  let arg = num in
  let action ~ident1 ~arg1 ~op1 ~arg2 () = (ident1, arg1, op1, arg2) in
  [%load_schemas "operation.schema", action]

let test str =
  let ((id, arg1, op, arg2), _) = Option.get (parse_op (explode str))
  in
  Printf.printf "%s =>\narg1 = %d, arg2 = %d, id = %s, op = %s\n\n" str arg1 arg2 id op

let () =
  List.iter test [
    "let x be the sum of 1 and 2";
    "let y be the sum of 12 and 567";
    "the sum of 0 and 1 results in y";
    "myvar is the product of 3 and 12"
  ]