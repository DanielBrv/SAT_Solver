(* open Tokens *)
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

(* Creates a Lit from var, Negates Lit if var is negative.
  Raises exception if var is 0 since 0 represents end of line *)
let int_to_lit num = if num > 0 then Var(num)
  else if num != 0 then Not(abs num)
  else raise (Failure "Zero cannot be a variable")

(* Parse a single DIMACS clause line into a list of ints, ignoring the trailing 0 *)
let parse_clause_line line =
  line

  |> List.filter (fun x -> x <> 0)
  |> List.map int_to_lit


let parse_clauses clause_lines num_clauses : cnf =
  let rec aux lines count acc =
    match lines, count with
    | [], _ | _, 0 -> List.rev acc
    | l :: rest, n ->
        let clause = parse_clause_line l in
        aux rest (n - 1) (clause :: acc)
  in
  aux clause_lines num_clauses []
;;


