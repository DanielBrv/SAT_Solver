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


type lit =
  | Var of var
  | Not of var

type clause = lit list


type cnf = clause list