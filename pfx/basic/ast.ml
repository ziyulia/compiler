type command =
|Push 
|Pop
|Swap
|Add 
|Div
|Mul
|Rem
|Sub;;

type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let string_of_command = function
|Push -> "push"
|Pop -> "pop"
|Swap -> "swap"
|Add -> "add"
|Div -> "div"
|Mul -> "mul"
|Rem -> "rem"
|Sub -> "sub";;

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds);;

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds);;

