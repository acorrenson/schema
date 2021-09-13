open Ppxlib
(* open Schema.Schemas *)

let read_data ~loc input =
  let dict = ref [] in
  try while true do
    dict := (input_line input |> String.trim |> Ast_builder.Default.estring ~loc)::!dict
  done; assert false
  with End_of_file -> List.rev !dict


let expand ~loc ~path:_ ({ pexp_desc; _ } : expression) =
  match pexp_desc with
  | Pexp_constant (Pconst_string (c, _, _)) ->
    let input = try open_in c with _ ->
      Ppxlib.Location.raise_errorf ~loc "unable to open the file %s !" c
    in
    let data = read_data ~loc input in
    close_in input;
    Ast_builder.Default.elist ~loc data
  | _ -> assert false

let expand_id ~loc ~path:_ ({ pexp_desc; _ } : expression) =
  match pexp_desc with
  | Pexp_constant (Pconst_string (x, _, _)) ->
    let mk_arg x = Ast_builder.Default.ppat_var ~loc {txt = x; loc} in
    let mk_var x = Ast_builder.Default.evar ~loc x in
    let args = [mk_arg x; mk_arg "x1"; mk_arg "x2"] in
    let ret = [%expr [%e mk_var x] + x1 + x2] in
    List.fold_left (fun acc x -> [%expr fun [%p x] -> [%e acc]]) ret args
  | _ -> assert false

let expand_schema ~loc ~path:_ ({ pexp_desc; _ } : expression) =
  match pexp_desc with
  | Pexp_constant (Pconst_string (txt, _, _)) ->
    let open (Ast_builder.Make (struct let loc = loc end)) in
    let open Schema.Schemas in
    let open Schema.Combinators in
    let (toks, _) = Option.get @@ schema (explode txt) in
    let ref_num = List.length @@ get_ref toks in
    let count = ref (-1) in
    let mk_var i = evar ("r" ^ string_of_int i) in
    let mk_tmp () =
      incr count; ppat_var {txt=("r" ^ string_of_int !count); loc}
    in
    let ret = [%expr return [%e pexp_tuple (List.init ref_num mk_var |> List.rev)]] in
    List.fold_left (fun acc e ->
      match e with
      | Word w -> [%expr let* _ = p_spaces *> token [%e estring w] in [%e acc]]
      | Ref r  -> [%expr let* [%p mk_tmp ()] = p_spaces *> [%e evar r] in [%e acc]]
    ) ret (List.rev toks)
    (* let mk_arg x = ppat_var { txt = x; loc } in
    [%expr 
      let* _ = p_spaces *> token "let" in
      let* r_op = p_spaces *> op in
      let* _ = p_spaces *> token "be" in
      let* r_arg = p_spaces *> arg in
      return (r_op, r_arg)
    ] *)
    (* let (res, _) = List.fold_left (fun (acc, i) e ->
      match e with
      | Word w -> [%expr let* _ = p_spaces *> token [%e estring w] in [%e acc]], i
      | Ref r -> [%expr let* [%p mk_tmp i] = p_spaces *> [%e evar r] in [%e acc]], i + 1
    ) ([%expr return [%e pexp_tuple [evar "r0"; evar "r1"]]], 0) (List.rev toks)
    in
    [%expr fun [%p mk_arg "op"] [%p mk_arg "arg"] -> [%e res]] *)
  | _ -> assert false

let extension_schema =
  Extension.declare
    "schema"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_schema

let extension =
  Extension.declare
    "load_dict"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let extension_id =
  Extension.declare
    "id"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_id

let rule1 = Context_free.Rule.extension extension
let rule2 = Context_free.Rule.extension extension_id

let rule3 = Context_free.Rule.extension extension_schema

let () =
  Driver.register_transformation ~rules:[rule1; rule2; rule3] "ppx_schema"