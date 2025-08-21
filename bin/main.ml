
open Satsolver_lib.Types


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
  | Not(a) -> "¬" ^ Printf.sprintf "%c" a
  | Var(a)-> Printf.sprintf "%c" a

let rec print_clause clause =
  match clause with
  | lit :: [] -> print_lit lit
  | lit :: t -> print_lit lit  ^ " ∨ " ^ print_clause t
  | [] -> ""


let rec print_cnf cnf =
  match cnf with
  | clause :: [] -> "(" ^ print_clause clause ^ ")"
  | clause :: t -> "(" ^ print_clause clause ^ ")" ^ " ∧ " ^ print_cnf t
  | [] -> "()"


let test_input = read_line ()
(*let test_input = "test Input"*)

let tokens = (Satsolver_lib.Lexer.lexer test_input)

let () = print_endline ("[" ^ (Satsolver_lib.Lexer.print_tokens tokens )^ "]")
let () = print_endline ("Tree: "^ print_cnf (Satsolver_lib.Parser.parse tokens) )
