open Source
open Values
open Eval
open Types

let memory_size mem host at _ =
  Some (Int32 (Int32.of_int (Memory.size mem)))

let page_size mem host at _ =
  Some (Int32 (Int32.of_int host.page_size))

let resize_memory mem host at vs =
  match vs with
  | Int32 i :: [] ->
    if (Int32.rem i (Int32.of_int host.page_size)) <> Int32.zero then
      Error.error at "runtime: resize_memory operand not multiple of page_size";
    Memory.resize mem (Int32.to_int i);
    None
  | _ ->
    Error.error at "runtime: resize_memory expects a single i32 parameter"

let match_import (i : Ast.import) =
  let {Ast.module_name; func_name; func_params; func_result} = i.it in
  let (result : Eval.import) = match module_name, func_name, func_result, func_params with
  | "wasm_intrinsics", "memory_size", Some { at = _; it = Int32Type}, [] -> memory_size
  | "wasm_intrinsics", "page_size", Some { at = _; it = Int32Type}, [] -> memory_size
  | "wasm_intrinsics", "resize_memory", None, { at = _; it = Int32Type} :: [] -> resize_memory
  | _, _, _, _ ->
    let func_params_it = List.map it func_params in
    let func_result_it = match func_result with
    | Some t -> Some t.it
    | None -> None
    in let func_type = { ins=func_params_it; out=func_result_it } in
    Error.error i.at ("no intrinsic \"" ^ func_name ^ "\" of type " ^ (string_of_func_type func_type))
  in result
