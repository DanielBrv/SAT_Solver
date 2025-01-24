(* e -> lit
   | ~e
   | e op e
   | (e)

   lit -> var
    | True
    | False
  
    op -> AND
    | OR
    | =>
    | <=>
*)
type var = char

type op = 
| AND
| OR
| Implication
| Biconditional

type formula =
| Lit of var
| Not of formula
| Clause of formula * op * formula
| SubFormula of formula
