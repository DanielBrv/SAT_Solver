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
type var = char

type atom = 
| Var of var
| True
| False

type lit = 
| Atom of atom
| Not of atom

type op = 
| AND
| OR
| Implication
| Biconditional

type clause =
| Lit of lit
| Clause of lit * op * clauses



type formula =
| Clause of clause * op * formula
| Formula of formula
