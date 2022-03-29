open Ast
open BasicPfx.Ast

let rec generate = function
  | Const x -> Push :: Num x :: []
  (* | Const _ -> failwith "Not int" *)

  | Binop(BinOp.Badd, e1 , e2) -> generate e1 @ generate e2 @ Add :: []

  | Binop(BinOp.Bsub, e1 , e2) ->  generate e1 @ generate e2 @ Swap :: Sub :: []

  | Binop(BinOp.Bmul, e1 , e2) -> generate e1 @ generate e2 @ Mul :: []
  
  | Binop(BinOp.Bdiv, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Div :: []

  | Binop(BinOp.Bmod, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Rem :: []

  (* | Binop(_,_,_) -> failwith "Not supported format" *)

  | Uminus e -> generate e @ Push :: Num 0 :: Sub :: []

  | Var _ -> failwith "Not yet supported"
  
  | Fun(_,e) -> Lambda (generate e) 
  | App(e1,e2) -> generate e1 @ generate e2