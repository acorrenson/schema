# schema

A minimalist Natural Language Processing module in OCaml

## What is schema ?

Schema is a small OCaml module designed for the parsing and analysis of plain text. It features a lightweight method to declare language patterns similar to regular expressions.
Such patterns are called schemas and are intended to be compiled down to efficient OCaml code via the use of the `Ppx_schema` syntax extension.

## How to use Schema

### Declare schemas

Suppose we want to parse sentences similar to *x is the sum of y and z* but where
*x*, *y* and *z* can be any sequence of words denoting a mathematical expression and *sum* may be any binary operation (e.g. *product*, *division*, *subtraction*, ...).
Providing we already defined a parser `expr` recognizing expressions and a parser `op` recognizing binary operation names, a good schema to parse such declarations would be
*$expr is the $operation of $expr and $expr*.

Schemas are intended to be compiled to parsers compliant with the `Schema.Combinators` module. Such parsers are of the form `char list -> (t, char list) option`
where `t` is the type of the structure we want to parse and can be integrated in more standard parsers built by hand using the `Combinators` api.
  
Compilation of schemas is performed at ocaml's compile time via the use of syntax extensions (so called Ppx).
To compile a schema an action should be provided. Actions are simply functions that are called each time a schema is detected. Sub-patterns discovered during the execution of a schema parser are given as labeled arguments to the action function.

```ocaml
let declaration =
  let op = operation_parser in
  let expr = expression_parser in
  let action ~expr1 ~op1 ~expr2 () = (op1, expr1, expr2) in
  [@schema "$expr is the $op of $expr and $expr", action]
```

Schemas can also be loaded from text files. This is especially convenient when you want to
declare several formulations of the same pattern.
For example, the patterns `the $op of $expr and $expr results in $expr` and `let $expr be the $op of $expr and $expr` are similar to
`$expr is the $op of $expr and $expr`. We can unify these 3 schemas by gathering them in a `declaration.schema` text file:

```
the $op of $expr and $expr results in $expr$
let $expr be the $op of $expr and $expr
$expr is the $op of $expr and $expr
```

and then we load and compile these schemas 

```ocaml
let declaration =
  let op = operation_parser in
  let expr = expression_parser in
  let action ~expr1 ~op1 ~expr2 () = (op1, expr1, expr2) in
  [@load_schemas "declaration.schema", action]
```

This is semantically equivalent to the following 

```ocaml
let declaration =
  let op = operation_parser in
  let expr = expression_parser in
  let action ~expr1 ~op1 ~expr2 () = (op1, expr1, expr2) in
  [@schema "the $op of $expr and $expr results in $expr$", action]
  <|>
  [@schema "let $expr be the $op of $expr and $expr", action]
  <|>
  [@schema "$expr is the $op of $expr and $expr", action]
```

However, we recommend the usage of `[@load_schemas]` because it allows to perform  optimizations of the generated parser instead of just combining them with the union operator.

### Compilation

Assuming Schema is installed, the easiest way to use Schema is via dune.

```dune
(library 
  (name MyLib)
  ...
  (preprocess (pps schema.ppx_schema))
  (libraries schema)
)

(executable 
  (name MyExec)
  ...
  (preprocess (pps schema.ppx_schema))
  (libraries schema)
)
```