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

let  parse_disjunct toks = 
  let literal, left_over = parse_literal toks in 
    match literal, left_over with
    | Var(x), [] -> Lit(literal), []
    | _, _ -> "c"
let rec parse_cnf toks = 
  let disjunction, left_over = parse_disjunct toks in 
    match disjunction, left_over with

    | _, _ -> "c"



(* let rec parse_literal toks = 
  match toks with
  | [] -> raise (Failure "parse error, No Tokens")
  | Tok_Not :: t -> 
    let sub_tree, left_over = parser t in
      Not(sub_tree), left_over
  | Tok_LParen :: Tok_Var(x) :: Tok_And :: Tok_Var(z) :: Tok_RParen :: t ->
      Clause(Lit(x), AND, Lit(z)), t
  | Tok_LParen :: Tok_Var(x) :: Tok_Or :: Tok_Var(z) :: Tok_RParen :: t ->
    Clause(Lit(x), OR, Lit(z)), t
  | Tok_LParen :: Tok_Var(x) :: Tok_RParen :: t -> 
      op_parse t SubFormula(Lit(x))
  | Tok_LParen :: t ->
    let sub_tree, left_over = parser t in
        op_parse left_over sub_tree (*replace function call with code its self*)
  | Tok_Var(x) :: Tok_RParen :: t ->
      Lit(x), t
  | Tok_Var(x) :: [] -> Lit(x)
  | Tok_Var(x) :: t -> op_parse t Lit(x)
*)
(*
let rec parser toks = 
  match toks with
  | [] -> raise (Failure "parse error, No Tokens")
  | Tok_Not :: t -> 
    let sub_tree, left_over = parser t in
      Not(sub_tree), left_over
  | Tok_LParen :: Tok_Var(x) :: Tok_And :: Tok_Var(z) :: Tok_RParen :: t ->
      Clause(Lit(x), AND, Lit(z)), t
  | Tok_LParen :: Tok_Var(x) :: Tok_Or :: Tok_Var(z) :: Tok_RParen :: t ->
    Clause(Lit(x), OR, Lit(z)), t
  | Tok_LParen :: Tok_Var(x) :: Tok_RParen :: t -> 
      op_parse t SubFormula(Lit(x))
  | Tok_LParen :: t ->
    let sub_tree, left_over = parser t in
        op_parse left_over sub_tree (*replace function call with code its self*)
  | Tok_Var(x) :: Tok_RParen :: t ->
      Lit(x), t
  | Tok_Var(x) :: [] -> Lit(x)
  | Tok_Var(x) :: t -> op_parse t Lit(x)
  *)



let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parse_cnf toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")