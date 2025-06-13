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

let op_parse toks left_sub =
  match toks with 
  | Tok_And :: t -> let sub_formula, left_over = parser t in
      let right_sub = sub_formula in
        Clause(left_tree, AND, right_sub), left_over
  | Tok_Or :: t -> raise (Failure " OR Not implemented")
  | Tok_Eq :: Tok_Gt :: t -> raise (Failure " => Not implemented")
  | Tok_Gt :: Tok_Eq :: Tok_Gt :: t -> raise (Failure " <=> Narser tot implemented")
  |_ -> raise (Failure "parse error, Illegal Operation")

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



let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parser toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")
