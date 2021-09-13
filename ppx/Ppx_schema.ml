open Ppxlib

let expand_schema ~loc ~path:_ ({ pexp_desc; _ } : expression) =
  match pexp_desc with
  | Pexp_constant (Pconst_string (txt, _, _)) ->
    Schema.Schemas.compile_schema ~loc txt
  | _ ->
    Location.raise_errorf ~loc "expected string"

let expand_load_schemas ~loc ~path:_ ({ pexp_desc; _ } : expression) =
  match pexp_desc with
  | Pexp_constant (Pconst_string (txt, _, _)) ->
    Schema.Schemas.load_schemas ~loc txt
  | _ ->
    Location.raise_errorf ~loc "expected file name"

let extension_schema =
  Extension.declare
    "schema"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_schema

let extension_load_schemas =
  Extension.declare
    "load_schemas"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_load_schemas


let () =
  Driver.register_transformation ~rules:[
    Context_free.Rule.extension extension_schema;
    Context_free.Rule.extension extension_load_schemas;
  ] "ppx_schema"