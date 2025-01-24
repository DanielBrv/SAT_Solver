open Types
(*
  Negation := "~"
  Conjunction := "^"
  Disjunction := "|"
  Implication := "=>"   Not implemented
  Bimplication := "<=>" Not implemented

  Example Input : ((a ^ b) | (~(c | ~d) ^ (h) ))
*)

(* e -> lit
   | ~e
   | e op e
   | (e)

   lit -> var
    | True
    | False
  
    op -> AND
    | OR
    | =>
    | <=>
*)

let print_tree tree =
  match tree with
  | AND -> "AND"
  | OR -> "OR"
  | Implication -> "Implication"
  | Biconditional -> "BiCond"


let test_input = read_line ()
(*let test_input = "test Input"*)

let tokens = (Lexer.lexer test_input 0 [])


let () = print_endline ("Tree: "^ print_tree (Parser.parse tokens) ^ "[" ^ (Lexer.print_tokens tokens )^ "]")
