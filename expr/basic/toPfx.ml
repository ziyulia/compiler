open Ast
open BasicPfx.Ast

  (* Question 5.2 (code):
  Define a function generate implementing the semantics *)

let rec generate = function
  | Const x -> Push :: Num x :: []

  | Binop(BinOp.Badd, e1 , e2) -> generate e1 @ generate e2 @ Add :: []

  | Binop(BinOp.Bsub, e1 , e2) ->  generate e1 @ generate e2 @ Swap :: Sub :: []

  | Binop(BinOp.Bmul, e1 , e2) -> generate e1 @ generate e2 @ Mul :: []
  
  | Binop(BinOp.Bdiv, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Div :: []

  | Binop(BinOp.Bmod, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Rem :: []

  | Uminus e -> generate e @ Push :: Num 0 :: Sub :: []

  | Var _ -> failwith "Not yet supported"
