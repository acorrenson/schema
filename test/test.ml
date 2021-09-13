open Schema
open Combinators
open Schemas

let () =
  List.iter print_endline [%load_dict "hello.txt"]

let () =
  Printf.printf "int => %d\n" ([%id "x"] 1 2 3)

let num = 
  (fun x -> implode x |> int_of_string) <$> (
    check (function '1'..'9' -> true | _ -> false)
    <~>
    many (check (function '0'..'9' -> true | _ -> false))
  )

let parser =
  let op = p_ident in
  let arg = num in
  [%schema "let $op be $arg in $op"]

let () =
  let ((a, b, c), _) = Option.get (parser (explode "let x be 42 in y")) in
  Printf.printf "%s %d %s\n" a b c
