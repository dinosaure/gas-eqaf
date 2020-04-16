open Rresult

external bool_of_int : int -> bool = "%identity"

let camlEqaf__equal_155 prgm v0 v1 =
  let heap = Heap.make ~len:(Heap.needed_string v0 + Heap.needed_string v1) in
  let rax_value = Heap.inject_string heap v0 in
  let rbx_value = Heap.inject_string heap v1 in
  let registers = Registers.make () in
  registers.Register.%[Register.rax] <- Int64.of_int rax_value ;
  registers.Register.%[Register.rbx] <- Int64.of_int rbx_value ;
  Asm.execute "camlEqaf__equal_155" ~registers ~heap prgm >>| fun ticks ->
  bool_of_int (Int64.to_int registers.Register.%[Register.rax] lsr 1), ticks


