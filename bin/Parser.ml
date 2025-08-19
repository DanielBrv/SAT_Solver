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
    | Tok_Var(x)::Tok_RParen::t  -> Var(x), t                 (* Parsing last variable of clause *)
    | Tok_Not :: Tok_Var(x)::Tok_RParen::t -> Not(Var(x)), t  (* Parsing last negation of clause *)
    | Tok_Var(x)::t  -> Var(x), t                             (* Parsing variable *)
    | Tok_Not :: Tok_Var(x)::t -> Not(Var(x)), t              (* Parsing negation *)
    | _ -> raise (Failure "Parsing literals failure")

let rec parse_disjunct toks = 
  let literal, left_over = parse_literal toks in 
    match literal, left_over with

    | _, Tok_Or::t -> let right_side, left_over = parse_disjunct t in
                        Or(literal, right_side), left_over (* Parsing right side of Or()*)
    | Var(_), t -> Lit(literal), t (* Returning Lit() *)
    | Not(_), t -> Lit(literal), t

let rec parse_cnf toks = 
  match toks with
  | Tok_LParen::t -> (let disjunction, left_over = parse_disjunct t in
      (* Parsing inside of parenthesis*)   
      match  left_over with
      | [] -> Clause(disjunction), []  (* Returning Clause *)
      | Tok_And::t -> let right_side, left_over = parse_cnf t in (* Parsing right side of And()*)
                        And(disjunction, right_side), left_over
      | _ -> raise (Failure "Parsing cnf failure"))
  |_ -> raise (Failure "Parsing cnf failure: missing left parenthesis ")


let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parse_cnf toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")
      