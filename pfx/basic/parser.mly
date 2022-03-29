%{
  (* Ocaml code here*)
  open Ast
%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
 (* question 9.3 *)
%token  EOF PUSH POP SWAP ADD DIV MUL REM SUB EXEC GET LPAR RPAR
%token <int> INT


(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%


(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)

program:
| i = INT e=expr EOF  { i, e }

expr:
  |PUSH  i=INT e = expr {Push::Num i::e}
  |POP   e = expr {Pop::e }
  |SWAP  e = expr {Swap::e}
  |ADD   e = expr {Add::e }
  |DIV   e = expr {Div::e }
  |MUL   e = expr {Mul::e }
  |REM   e = expr {Rem::e }
  |SUB   e = expr {Sub::e }
  |EXEC  e = expr {Exec::e}
  |GET   e = expr {Get ::e}
  |e = simple_expr e1 = expr {Lambda e::e1}
  |PUSH  i=INT {Push::Num i::[]}
  |POP   {Pop::[] }
  |SWAP  {Swap::[]}
  |ADD   {Add::[] }
  |DIV   {Div::[] }
  |MUL   {Mul::[] }
  |REM   {Rem::[] }
  |SUB   {Sub::[] }
  |EXEC  {Exec::[]}
  |GET   {Get ::[]}
  |e = simple_expr {Lambda e::[]}
  
simple_expr:
  |LPAR e=expr RPAR           { e }

  %%
