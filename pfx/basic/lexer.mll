{
  open Parser
  (* type token =
  |Push 
  |Pop
  |Swap
  |Add 
  |Div
  |Mul
  |Rem
  |Sub
  (* |EOF *)
  |Num of int;; 

  let print_token = function
  (* |EOF -> print_string "EOF" *)
  |Push -> print_string "push"
  |Pop -> print_string "pop"
  |Swap -> print_string "swap"
  |Add -> print_string "add"
  |Div -> print_string "div"
  |Mul -> print_string "mul"
  |Rem -> print_string "rem"
  |Sub -> print_string "sub"
  |Num i -> print_int i ;; *)

  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* | eof      { EOF } *)
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  | "push"    {PUSH}
  | "add"    {ADD}
  | "div"    {DIV}
  | "sub"    {SUB}
  | "rem"    {REM}
  | "pop"    {POP}
  | "swap"    {SWAP}
  | "mul"    {MUL}
  (* question 9.3 *)
  | "("       {LPAR}
  |")"        {RPAR}
  |"exec"     {EXEC}
  |"get"      {GET}
  
  (* illegal characters *)
  (* Exercise 7  *)
  (* Modify  code from the previous exercise to be able to return the location of errors. *)
  | _ as c                  { failwith (Printf.sprintf "Error at position %s %c" (string_of_int lexbuf.lex_curr_pos) c) }

{
  let rec examine_all lexbuf =
    let result = token lexbuf in
    (* print_token result; *)
    print_string " ";
    match result with
    | EOF -> ()
    | _   -> examine_all lexbuf

  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

  let _ = Arg.parse [] compile ""
}