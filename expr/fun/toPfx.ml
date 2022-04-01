open Ast
open BasicPfx.Ast

(* Question 10.3 (code):
Provide a new version of generate. *)

let rec generate_exec_seq = function 
  | Const x,n -> Push :: Num x :: [],n+1 
  (* | Const _ -> failwith "Not int" *)

  | Binop(BinOp.Badd, e1 , e2),n -> 
    let pfx1,n1 = generate_exec_seq (e1,n) in (
    let pfx2,n2 = generate_exec_seq (e2,n1) in pfx1 @ pfx2 @ Add :: [],n2-1)

  | Binop(BinOp.Bsub, e1 , e2),n -> 
    let pfx2,n2 = generate_exec_seq (e2,n) in (
    let pfx1,n1 = generate_exec_seq (e1,n2) in  pfx2 @ pfx1 @ Sub :: [],n1-1)

  | Binop(BinOp.Bmul, e1 , e2),n ->
    let pfx1,n1 = generate_exec_seq (e1,n) in (
    let pfx2,n2 = generate_exec_seq (e2,n1) in  pfx1 @ pfx2 @ Mul :: [],n2-1 )
     
  | Binop(BinOp.Bdiv, e1 , e2),n -> 
    let pfx2,n2 = generate_exec_seq (e2,n) in (
    let pfx1,n1 = generate_exec_seq (e1,n2) in  pfx2 @ pfx1 @ Div :: [],n1-1 )

  | Binop(BinOp.Bmod, e1 , e2),n -> 
    let pfx2,n2 = generate_exec_seq (e2,n) in (
    let pfx1,n1 = generate_exec_seq (e1,n2) in  pfx2 @ pfx1 @ Rem :: [],n1-1 )

  | Uminus e,n -> 
    let pfx,n1 = generate_exec_seq (e,n) in pfx @ Push :: Num 0 :: Sub :: [],n1

  | Var _ , n -> Push :: Num (n+1) :: Get ::[],n+1 
  
  | App(e1,e2),n -> 
    let pfx2,n2 = generate_exec_seq (e2,n) in (
    let pfx1,_ = generate_exec_seq (e1,n2) in  pfx2 @ Lambda pfx1 :: Exec :: [],n2 )

  | Fun(_,e),n ->  let pfx,_ = generate_exec_seq (e,0) in pfx, n;;


let rec generate = function
  | Const x -> Push :: Num x :: []

  | Binop(BinOp.Badd, e1 , e2) -> generate e1 @ generate e2 @ Add :: []

  | Binop(BinOp.Bsub, e1 , e2) ->  generate e1 @ generate e2 @ Swap :: Sub :: []

  | Binop(BinOp.Bmul, e1 , e2) -> generate e1 @ generate e2 @ Mul :: []
  
  | Binop(BinOp.Bdiv, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Div :: []

  | Binop(BinOp.Bmod, e1 , e2) -> generate e1 @ generate e2 @ Swap :: Rem :: []

  | Uminus e -> generate e @ Push :: Num 0 :: Sub :: []

  | Var _ -> failwith "Not yet supported"
  
  | App(e1,e2) -> generate e2 @ Lambda (generate e1) :: Exec :: []

  | Fun(_,e) -> let x,_ = generate_exec_seq (e,0) in x;;


