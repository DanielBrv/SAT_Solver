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

let rec parse_clause toks =
  let literal, left_over = parse_literal toks in
  match left_over with
  | Tok_Or :: t ->
      let rest_clause, remaining = parse_clause t in
      (literal :: rest_clause, remaining)   (* prepend literal to the rest of the clause *)
  | _ ->
      ([literal], left_over)               (* single literal clause *)

(* parse_cnf : token list -> cnf * token list *)
let rec parse_cnf toks =
  match toks with
  | [] -> ([], [])  (* empty CNF *)
  | Tok_LParen :: t ->
      let clause, left_over = parse_clause t in (
        match left_over with
        | Tok_RParen :: Tok_And :: t ->
          let rest_cnf, remaining = parse_cnf t in
          (clause :: rest_cnf, remaining)  (* prepend clause to CNF list *)
        | Tok_RParen :: t ->
          ([clause], t)  (* last clause, return as singleton list *)
        | _ -> raise (Failure "Parsing CNF failure: expected closing parenthesis or AND")
        )
  | _ -> raise (Failure "Parsing CNF failure: missing left parenthesis")

let parse toks =
  if toks = [] then raise (Failure "Nothing to parse, Empty Input")
  else
    let parse_tree, left_over = parse_cnf toks in
      if left_over = [] then (parse_tree)
      else raise (Failure "Parsing incomplete, Left over tokens")
      