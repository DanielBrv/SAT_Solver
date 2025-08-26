open Types

let simplify cnf lit=
      let assign =
        match lit with
        | Var v -> (v, true)
        | Not v -> (v, false)
      in
        List.filter_map
          (fun clause ->
            if List.exists (fun l -> l = lit) clause then
              None  (* whole clause satisfied, drop it *)
            else
              let reduced =
                List.filter
                  (fun l ->
                    l <> (match lit with
                          | Var v -> Not v
                          | Not v -> Var v))
                  clause
              in
              Some reduced)
          cnf
      (* recurse with updated CNF and model *)
      , assign

let rec find_unit_clause (clauses : clause list) =
    match clauses with
    | [] -> None
    | [lit] :: _ -> Some lit
    | _ :: rest -> find_unit_clause rest
(* unit_propagate returns a simplified CNF and the accumulated assignment *)
let rec unit_propagate (cnf : clause list) (model : assignment list)  =
  (* find a unit clause if it exists *)
  match find_unit_clause cnf with
  | None -> (cnf, model)  (* nothing more to propagate *)
  | Some lit ->
      (* convert literal to (var, value) for the model *)
      (* simplify the CNF with respect to lit *)
      let simplified, assigned = simplify cnf lit in
      (* recurse with updated CNF and model *)
        unit_propagate simplified (assigned :: model)

(* 
(* unit_propagate returns a simplified CNF and the accumulated assignment *)
let rec unit_propagate (cnf : clause list) (model : assignment list)  =
  (* find a unit clause if it exists *)
  let rec find_unit_clause (clauses : clause list) =
    match clauses with
    | [] -> None
    | [lit] :: _ -> Some lit
    | _ :: rest -> find_unit_clause rest
  in

  match find_unit_clause cnf with
  | None -> (cnf, model)  (* nothing more to propagate *)
  | Some lit ->
      (* convert literal to (var, value) for the model *)
      let assign =
        match lit with
        | Var v -> (v, true)
        | Not v -> (v, false)
      in
      (* simplify the CNF with respect to lit *)
      let simplified =
        List.filter_map
          (fun clause ->
            if List.exists (fun l -> l = lit) clause then
              None  (* whole clause satisfied, drop it *)
            else
              let reduced =
                List.filter
                  (fun l ->
                    l <> (match lit with
                          | Var v -> Not v
                          | Not v -> Var v))
                  clause
              in
              Some reduced)
          cnf
      in
      (* recurse with updated CNF and model *)
      unit_propagate simplified (assign :: model)

*)
let pure_lit_elimination = ()

let dpll (cnf: clause list) (model : assignment list ) = 
  match cnf with
  | [] -> Sat(model)
  | []::_ -> Unsat
  | _ -> Unsat
  (*
  | _ -> let reduced, model = unit_propagate2 cnf model in 
  *)