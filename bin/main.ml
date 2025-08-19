
open Satsolver_lib.Types

(*
  Negation := "~"
  Conjunction := "^"
  Disjunction := "|"
  Implication := "=>"   Not implemented
  Bimplication := "<=>" Not implemented

  Example Input : ((a ^ b) | (~(c | ~d) ^ (h) ))
*)

(* 
   Conjunctive normal form
  CNF       → Disjunct
           | Disjunct ∧ CNF

  Disjunct  → Literal
           | Literal ∨ Disjunct

  Literal   → Variable
           | ¬ Literal
*)


let print_lit tree =
  match tree with
  | Not(Var(a)) -> "¬" ^ Printf.sprintf "%c" a
  | Var(a)-> Printf.sprintf "%c" a
  | _ -> raise (Failure "Printing Literal Failure")

let rec print_disjunct tree =
  match tree with
  | Or(a,b) -> " " ^ print_lit a ^ " ∨" ^ print_disjunct b
  | Lit(a) -> " " ^ print_lit a ^ " "


let rec print_cnf tree =
  match tree with
  | And(a,b) -> "(" ^ print_disjunct a ^ ")" ^ " ∧ " ^ print_cnf b
  | Clause(a) -> "(" ^ print_disjunct a ^ ")"


let test_input = read_line ()
(*let test_input = "test Input"*)

let tokens = (Satsolver_lib.Lexer.lexer test_input)

let () = print_endline ("[" ^ (Satsolver_lib.Lexer.print_tokens tokens )^ "]")
let () = print_endline ("Tree: "^ print_cnf (Satsolver_lib.Parser.parse tokens) )
