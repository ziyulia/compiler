(* The type of the commands for the stack machine *)
type command =
|Push 
|Pop
|Swap
|Add 
|Div
|Mul
|Rem
|Sub
|Num of int
|Exec
|Get
|Lambda of command list;;

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
