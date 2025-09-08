
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

let rec print_model (model : model) : string =
  match model with
  | (v,b) :: [] -> "( "^ Printf.sprintf "%c" v ^ " -> " ^ string_of_bool b ^" )"
  | (v,b) :: rest -> "( "^ Printf.sprintf "%c" v ^ " -> " ^ string_of_bool b ^ " ), " ^ print_model rest
  | [] -> ""

let print_result (result : result) =
  match result with
  | Sat(model) -> (print_endline ("Sat" ^ print_model model))
  | Unsat -> print_endline "Unsat"              


let test_input = read_line ()
(*let test_input = "test Input"*)

let tokens = (Satsolver_lib.Lexer.lexer test_input)

let () = print_endline ("[" ^ (Satsolver_lib.Lexer.print_tokens tokens )^ "]")

let cnf = Satsolver_lib.Parser.parse tokens

let () = print_endline ("Tree: "^ print_cnf cnf )
let result = Satsolver_lib.Solver.dpll cnf []
let () = print_result result
