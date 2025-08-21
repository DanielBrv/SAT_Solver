open Tokens
open Types
(* 
   Conjunctive normal form
  CNF       → Disjunct
           | Disjunct ∧ CNF

  Disjunct  → Literal
           | Literal ∨ Disjunct

  Literal   → Variable
           | ¬ Literal
*)


let parse_literal toks = 
  match toks with
    | Tok_Var(x)::t  -> Var(x), t                             (* Parsing variable *)
    | Tok_Not :: Tok_Var(x)::t -> Not(x), t              (* Parsing negation *)
    | _ -> raise (Failure "Parsing literals failure")

let rec parse_disjunct toks = 
  let literal, left_over = parse_literal toks in 
    match left_over with
    | Tok_Or::t -> let right_side, left_over = parse_disjunct t in
                        Or(literal, right_side), left_over (* Parsing right side of Or()*)
    | t -> Lit(literal), t (* Returning Lit() *)


let rec parse_cnf toks = 
  match toks with
  | Tok_LParen::t -> (let disjunction, left_over = parse_disjunct t in
      (* Parsing inside of parenthesis*)   
      match left_over with
      | Tok_RParen::Tok_And::t -> let right_side, left_over = parse_cnf t in (* Parsing right side of And()*)
                        And(disjunction, right_side), left_over
      | Tok_RParen::[] -> Clause(disjunction), []  (* Returning Clause *)
      | _ -> raise (Failure "Parsing cnf failure"))
  |_ -> raise (Failure "Parsing cnf failure: missing left parenthesis ")


let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parse_cnf toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")
      