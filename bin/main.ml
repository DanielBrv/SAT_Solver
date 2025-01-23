(*
  Negation := "~"
  Conjunction := "^"
  Disjunction := "|"
  Implication := "=>"   Not implemented
  Bimplication := "<=>" Not implemented

  Example Input : ((a ^ b) | (~(c | ~d) ^ (h) ))
*)

(* e -> lit
   | ~lit
   | ~(e)
   | (e)
   | (e op e)

   lit -> var
    | True
    | False
  
    op -> AND
    | OR
    | =>
    | <=>
*)
let test_input = read_line ()
(*let test_input = "test Input"*)

let tokens = (Lexer.lexer test_input 0 [])
let () = print_endline ("[" ^ (Lexer.print_tokens tokens )^ "]")
