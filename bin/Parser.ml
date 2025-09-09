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
  |> Str.split (Str.regexp "[ \t\n\r]+")   (* split on any whitespace *)
  |> List.map int_of_string
  |> List.filter (fun x -> x <> 0)
  |> List.map int_to_lit

let parse_header line =
  let tokens = String.split_on_char ' ' line |> List.filter ((<>) "") in
  match tokens with
  | ["p"; "cnf"; vars; clauses] ->
      let num_vars = int_of_string vars in
      let num_clauses = int_of_string clauses in
      num_vars, num_clauses
  | _ -> raise (Failure "Invalid Header")

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

let parse_dimacs lines = 
  match lines with
  | [] -> raise (Failure "Invalid Header")
  | header :: clauses -> let (num_vars, num_clauses) = parse_header header in 
    (parse_clauses clauses num_clauses,  num_vars, num_clauses)

