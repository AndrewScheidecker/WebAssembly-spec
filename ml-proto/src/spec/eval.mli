(*
 * (c) 2015 Andreas Rossberg
 *)

type instance
type value = Values.value
type host_params = {page_size : Memory.size}
type import = Memory.t -> host_params -> Source.region -> value list -> value option

val init : Ast.modul -> import list -> host_params -> instance
val invoke : instance -> string -> value list -> value option
  (* raise Error.Error *)
val eval : Ast.expr -> value option (* raise Error.Error *)
