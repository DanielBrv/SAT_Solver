
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

let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []
;;
let print_var var = "x" ^ string_of_int var
let print_lit tree =
  match tree with
  | Not(a) -> "¬" ^ print_var a 
  | Var(a)-> print_var a 

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
  | (v,b) :: [] -> "( "^ print_var v ^ " -> " ^ string_of_bool b ^" )"
  | (v,b) :: rest -> "( "^ print_var v ^ " -> " ^ string_of_bool b ^ " ), " ^ print_model rest
  | [] -> ""

let print_result (result : result) =
  match result with
  | Sat(model) -> (print_endline ("Sat" ^ print_model model))
  | Unsat -> print_endline "Unsat"              

let dimacs = read_lines "cnf.txt"

let print_lines lines =
  List.iter print_endline lines
;;

let () = print_lines dimacs

(*let test_input = read_line ()
let test_input = "test Input"*)
(*
let tokens = (Satsolver_lib.Lexer.lexer test_input)

let () = print_endline ("[" ^ (Satsolver_lib.Lexer.print_tokens tokens )^ "]")
*)
let cnf, _, _ = Satsolver_lib.Parser.parse_dimacs dimacs


let () = print_endline ("Tree: "^ print_cnf cnf )
let result = Satsolver_lib.Solver.dpll cnf []
let () = print_result result
