open Source

let print host mem at vs =
  List.iter Print.print_value (List.map (fun v -> Some v) vs);
  None

let match_import i =
  let {Ast.module_name; func_name; func_params; func_result} = i.it in
  match module_name, func_name with
  | "stdio", "print" ->
    if func_result <> None then
      Error.error i.at "stdio.print has no result";
    print
  | "wasm_intrinsics", _ ->
    Intrinsics.match_import i
  | _ ->
    Error.error i.at ("no import \"" ^ func_name ^ "\" from module \"" ^ module_name ^ "\"")

let match_imports (is : Ast.import list) =
  List.map match_import is
