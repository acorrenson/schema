open Schema
open Combinators
open Schemas

let num = 
  (fun x -> implode x |> int_of_string) <$> (
    many1 (check (function '0'..'9' -> true | _ -> false))
  )

let name = p_ident

type kl_val =
  | Sum of kl_val * kl_val
  | Var of string
  | Arg of int
  | Cst of int

type kl_constraint =
  | Ret of kl_val
  | Let of string * kl_val
  | Args of int

let rec pp_klval fmt = function
  | Sum (a, b) -> Format.fprintf fmt "(%a + %a)" pp_klval a pp_klval b
  | Var s -> Format.pp_print_string fmt s
  | Arg i -> Format.fprintf fmt "x%d" i
  | Cst i -> Format.pp_print_int fmt i

let print_constraint fmt = function
  | Ret v -> Format.fprintf fmt "it returns %a" pp_klval v
  | Let (x, v) -> Format.fprintf fmt "let %s = %a" x pp_klval v
  | Args i -> Format.fprintf fmt "it has %d arguments" i

let klcst = (fun x -> Cst x) <$> num

let klvar = (fun x -> Var x) <$> p_ident

let klarg =
  let action ~num1 () = Arg num1 in
  [%schema "the $num argument", action]

let klval =
  let operand = klarg <|> klcst <|> klvar in
  let action ~operand1 ~operand2 () = Sum (operand1, operand2) in
  [%schema "the sum of $operand and $operand", action]

let ret_clause =
  let action ~klval1 () = Ret klval1 in
  [%schema "it returns $klval", action]

let () =
  Schema.Sanitizer.sanitize "it returns the sum of 1 and argument 1"
  |> List.iter (fun sentence -> 
      let (cons, _) = Option.get (ret_clause (explode sentence)) in
      Format.printf "%a\n" print_constraint cons
    )



(* let parser =
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
  ] *)