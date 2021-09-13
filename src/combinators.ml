(** 
  {1 Combinators}

  A collection of basic combinators to design parsers
*)

(** {2 Basic parsers} *)

(**
  A [('a, 'b)] parser is a chunk of code that 
  analyses a sequence of ['a] and produces a ['b]
*)
type ('a, 'b) parser = 'a list -> ('b * 'a list) option

(**
  [check pred] is a simple [('a, 'a)] parser that
  detect an ['a] satisfying [pred] and returns it
*)
let check (pred : 'a -> bool) : ('a, 'a) parser = function
  | [] -> None
  | x::xs ->
    if pred x then Some (x, xs) else None

(** [return x] is the identity parser that returns [x] and does nothing else *)
let return value input = Some (value, input)

(** {2 Combinators} *)

(**
  [let* x = parser1 in parser2] is a parser performing the [parser1] analysis,
  storing its result as [x] and then the performing the [parser2]
  analysis (where [parser2] may depend on the value of [x])
*)
let (let*) (p : ('a, 'b) parser) (f : 'b -> ('a, 'c) parser) : ('a, 'c) parser = fun input ->
  match p input with
  | None -> None
  | Some (res, next) -> f res next

(** Map parser *)
let (<$>) (f : 'b -> 'c) (schm : ('a, 'b) parser) : ('a, 'c) parser =
  let* res = schm in
  return (f res)

(** Application parser *)
let (<*>) (sc1 : ('a, 'b -> 'c) parser) (sc2 : ('a, 'b) parser) : ('a, 'c) parser =
  let* f = sc1 in
  let* x = sc2 in
  return (f x)

(** Ignore left parser *)
let ( *> ) (sc1 : ('a, 'b) parser) (sc2 : ('a, 'c) parser) : ('a, 'c) parser =
  let* _ = sc1 in sc2

(** Ignore right parser *)
let ( <* ) (sc1 : ('a, 'b) parser) (sc2 : ('a , 'c) parser) : ('a, 'b) parser =
  let* res = sc1 in
  let* _ = sc2 in
  return res

(** Choice parser *)
let (<|>) (sc1 : ('a, 'b) parser) (sc2 : ('a, 'b) parser) : ('a, 'b) parser = fun input ->
  match sc1 input with
  | None -> sc2 input
  | Some _ as res -> res

let (<~>) p ps =
  let* x = p in
  let* xs = ps in
  return (x::xs)

let choice l = List.fold_left (<|>) (fun _ -> None) l

let rec many sc =
  let repeat =
    let* x = sc in
    let* xs = many sc in
    return (x::xs)
  in repeat <|> return []

let many1 sc = sc <~> many sc

(** {2 String utilities} *)

let implode x =
  String.concat "" @@ List.map (String.make 1) x

let explode x =
  List.init (String.length x) (String.get x)

let token (s : string) =
  implode <$> List.fold_left 
    (fun acc c -> check ((=) c) <~> acc)
    (return [])
    (explode s |> List.rev)