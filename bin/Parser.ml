open Tokens
open Types
(* 
   Conjunctive normal form
   Atom -> Var 
   | T
   | F

   lit -> var
    | Atom
    | -Atom
  
    Formula -> Clause AND Formula
    
    Clause -> Lit
    | Lit OR Clause
*)


let parse_literal toks = 
  match toks with
    | Tok_Var(x)::Tok_RParen::t  -> Var(x), t
    | Tok_Not :: Tok_Var(x)::t -> Not(Var(x)), t
    | _ -> raise (Failure "Parsing literals failure")

let rec parse_disjunct toks = 
  let literal, left_over = parse_literal toks in 
    match literal, left_over with
    | Var(_), [] -> Lit(literal), []
    | Not(_), [] -> Lit(literal), []
    | _, Tok_Or::t -> let right_side, left_over = parse_disjunct t in
                        Or(literal, right_side), left_over
    | _, _ -> raise (Failure "Parsing disjunction failure")
let rec parse_cnf toks = 
  let disjunction, left_over = parse_disjunct toks in 
    match disjunction, left_over with
    | Lit(_), [] -> Clause(disjunction), []
    | Or(_,_), [] -> Clause(disjunction), []
    | _, Tok_And::t -> let right_side, left_over = parse_cnf t in
                        And(disjunction, right_side), left_over
    | _, _ -> raise (Failure "Parsing cnf failure")


let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parse_cnf toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")