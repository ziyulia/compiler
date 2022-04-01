{
  open Parser
  open Utils
   let mk_int nb loc =
    try INT (int_of_string nb)
    with Failure _ -> raise (Location.Error(Printf.sprintf "Illegal integer '%s': " nb,loc))
}
(* 
Question 6.1 (code):
Write a lexer for the Pfx stack machine language. *)

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
  | digit+ as nb           { mk_int nb (Location.curr lexbuf)}
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
  | _ as c                  { raise (Location.Error(Printf.sprintf "Illegal character '%c': " c, Location.curr lexbuf)) }


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