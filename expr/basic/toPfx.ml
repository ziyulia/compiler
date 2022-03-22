open Ast

let rec generate = function
  | Const x -> Push x
  | Const _ -> failwith "Not int"

  | Binop(BinOp.Badd, e1 , e2) -> enerate e1 :: generate e1 :: Add :: []

  | Binop(BinOp.Bsub, e1 , e2) ->  generate e1 :: generate e2 ::Swap :: Sub :: []

  | Binop(BinOp.Bmul, e1 , e2) -> generate e1 :: generate e2 :: Mul :: []
  
  | Binop(BinOp.Bdiv, e1 , e2) -> generate e1 :: generate e2 ::Swap :: Div :: []

  | Binop(BinOp.Bmod, e1 , e2) -> generate e1 :: generate e2 ::Swap :: Rem :: []

  | Binop(_,_,_) -> failwith "Not supported format"

  | Uminus e -> generate e :: Push 0 :: Sub :: []

  | Var _ -> failwith "Not yet supported"