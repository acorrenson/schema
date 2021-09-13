open Ppxlib

let expand_schema ~loc ~path:_ txt action =
    Schema.Schemas.compile_schema ~loc txt action

let expand_load_schemas ~loc ~path:_ fname action =
  Schema.Schemas.load_schemas ~loc fname action
  
let extension_schema =
  Extension.declare
    "schema"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_tuple ((estring __)^::__^::nil)))
    expand_schema

let extension_load_schemas =
  Extension.declare
    "load_schemas"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_tuple ((estring __)^::__^::nil)))
    expand_load_schemas


let () =
  Driver.register_transformation ~rules:[
    Context_free.Rule.extension extension_schema;
    Context_free.Rule.extension extension_load_schemas;
  ] "ppx_schema"