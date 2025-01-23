open Tokens
open Stdlib
let is_whitespace char =
  match char with
  |' ' -> true
  |'\n' -> true
  |'\r' -> true
  |'\t' -> true
  |_ -> false

(* Returns String of representation of a token*)
let string_of_token t =
  match t with
  | Tok_RParen -> "Tok_RParen"
  | Tok_LParen -> "Tok_LParen"
  | Tok_And -> "Tok_And"
  | Tok_Or -> "Tok_Or"
  | Tok_Gt -> "Tok_Gt"
  | Tok_Lt -> "TokLt"
  | Tok_Eq -> "Tok_Eq"
  | Tok_Not -> "Tok_Not"
  | Tok_Var(value) -> "Tok_Var(" ^(String.make 1 value) ^ ")"

(* Checks that chars can be used as a valid variable  *)
let is_lexable_char char = 
  match char with
  | 'A'..'Z' -> char
  | 'a'..'z' -> char
  | _ -> raise (Failure ("Invalid variable name: " ^ (String.make 1 char)))

let char_to_token t =
  match t with
  | '(' -> Tok_RParen
  | ')' -> Tok_LParen
  | '^' -> Tok_And
  | '|' -> Tok_Or
  | '~' -> Tok_Not
  | '<' -> Tok_Lt
  | '=' -> Tok_Eq
  | '>' -> Tok_Gt
  | value -> Tok_Var((is_lexable_char value))
    
(*  gets the char at index pos in input and returns its token representation *)
(* potential optimization: change input from a string to the char *)
let lexer_help input pos =
  let length = String.length input in
    if pos >= length then raise (Failure "tokenizing failed")
    else char_to_token (String.get input pos)

let rec lexer input pos tokens = 
  let length = String.length input in
    if length = 0 then tokens
    else if pos > length - 1 then tokens (* length is less than 0*)
      
    else if pos > length - 1 || pos < 0 then
      raise (Failure ("Illegal lexing Pos: " ^ string_of_int pos)) (* length is less than 0*)

    else if is_whitespace (String.get input pos) then
      lexer input (pos + 1) tokens @ [] (* Checks if current char is a white space and ignores it*)

    else if length - 1 = pos then
      tokens @ [lexer_help input pos] (* base case: if theres a single char in input*)

    else lexer input (pos + 1) tokens @ [lexer_help input pos] (* recursive call*)

let rec print_tokens tokens = 
  match tokens with 
  | x :: y ->  (print_tokens y) ^ "" ^(string_of_token x) 
  | [] -> ""