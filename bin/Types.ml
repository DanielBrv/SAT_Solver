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


type result =
  | Sat of assignment
  | Unsat
  
type lit =
  | Var of var
  | Not of var
  | True
  | False

type clause = lit list


type cnf = clause list