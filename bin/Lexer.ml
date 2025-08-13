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
  | '(' -> Tok_LParen
  | ')' -> Tok_RParen
  | '&' -> Tok_And
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

let lexer input =
  let length = String.length input in
  let rec aux pos acc =
    if pos >= length then
      List.rev acc
    else if is_whitespace input.[pos] then
      aux (pos + 1) acc
    else
      aux (pos + 1) (lexer_help input pos :: acc)
  in
  aux 0 []



let rec print_tokens tokens = 
  match tokens with 
  | x :: y -> (string_of_token x) ^ "" ^ (print_tokens y)
  | [] -> ""