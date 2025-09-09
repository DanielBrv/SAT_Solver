(* 
   Conjunctive normal form
  CNF       → Disjunct
           | Disjunct ∧ CNF

  Disjunct  → Literal
           | Literal ∨ Disjunct

  Literal   → Variable
           | ¬ Literal
*)



type var = int

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
type cnf_meta = (cnf * int * int)