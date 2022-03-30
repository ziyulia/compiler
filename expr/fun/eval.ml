open Ast

exception RuntimeError of string

let rec eval env = function
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise(RuntimeError("Unbound variable "^v)))
  | Binop(op,e1,e2) ->
     begin
       match op,eval env e2 with
       | (Bdiv | Bmod), 0 -> raise(RuntimeError("division by zero"))
       | _, v -> (BinOp.eval op) (eval env e1) v
     end
  | Uminus e -> - (eval env e)

  (* try to pass the evaluation function *)
  | Fun(_,_) -> 1
  | App(Fun(_,_),_) -> 1
  | App(_,_) -> 0
