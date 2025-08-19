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
  | Not of lit


type disjunct = 
  | Lit of lit
  | Or of lit * disjunct
  (* Or should be "disjunct * disjunct"*)

type cnf = 
  | Clause of disjunct
  | And of disjunct * cnf