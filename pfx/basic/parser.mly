%{
  (* Ocaml code here*)
  open Ast
%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token  EOF PUSH POP SWAP ADD DIV MUL REM SUB 
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
|POP   e = expr {Pop::e}
|SWAP  e = expr {Swap::e}
|ADD   e = expr {Add::e}
|DIV   e = expr {Div::e}
|MUL   e = expr {Mul::e}
|REM   e = expr {Rem::e}
|SUB   e = expr {Sub::e}
|EOF {[]}


%%
