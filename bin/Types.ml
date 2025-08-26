(* 
   Conjunctive normal form
  CNF       → Disjunct
           | Disjunct ∧ CNF

  Disjunct  → Literal
           | Literal ∨ Disjunct

  Literal   → Variable
           | ¬ Literal
*)



type var = char

type assignment = (var * bool)

type model = assignment list 

type result =
  | Sat of model
  | Unsat

type lit =
  | Var of var
  | Not of var

type clause = lit list


type cnf = clause list