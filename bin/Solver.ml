open Types

(* Returns a list of all pure literals in a CNF *)
let find_pure_literals (cnf : cnf) : lit list =
  (* flattens all clauses into a single list containing literals *)
  let rec gather acc = function
    | [] -> acc
    | clause :: rest -> gather (clause @ acc) rest
  in
  let all_lits = gather [] cnf in

  (* Extracting all variables that appear positively i.e Var(x)*)
  let vars_pos = List.fold_left (fun s l ->
                    match l with
                    | Var v -> v :: s
                    | _ -> s) [] all_lits in

  (* Extracting all variables that appear negatively i.e Not(x) *)                  
  let vars_neg = List.fold_left (fun s l ->
                    match l with
                    | Not v -> v :: s
                    | _ -> s) [] all_lits in

  (* Combining and removing duplicates *)
  let unique_vars = List.sort_uniq compare (vars_pos @ vars_neg) in
  
  (* Filter out pure literals *)
  List.fold_left (fun acc v ->
    match List.mem v vars_pos, List.mem v vars_neg with
    | true, false -> Var v :: acc
    | false, true -> Not v :: acc
    | _ -> acc) [] unique_vars

let simplify (cnf : cnf) (lit : lit) : cnf * assignment =
      (* Creating assignment for lit *)
      let assign =
        match lit with
        | Var v -> (v, true)
        | Not v -> (v, false)
      in
      (* simplfied cnf*)
      let cnf' =
        (List.filter_map
          (fun clause ->
            if List.exists (fun l -> l = lit) clause then
              None  (* Literal results in true, whole clause satisfied, drop it *)
            else
              let reduced =
                (* Removing clauses containing negation of literal *)
                List.filter
                  (fun l ->
                    l <> (match lit with
                          | Var v -> Not v
                          | Not v -> Var v))
                  clause
              in
              Some reduced)
          cnf)
      in
      cnf' , assign

(* Finds first instance of a unit literal inside of a cnf and returns it. Returns None is not found.*)
let rec find_unit_clause (cnf : cnf) =
    match cnf with
    | [] -> None
    | [lit] :: _ -> Some lit (* matching clause that only contains a single lit*)
    | _ :: cnf' -> find_unit_clause cnf' (* recurse *)

(* unit_propagate returns a simplified CNF and the accumulated assignment *)
let rec unit_propagate (cnf : cnf) (model : model) : cnf * model  =
  (* find a unit clause if it exists *)
  match find_unit_clause cnf with
  | None -> (cnf, model)  (* nothing more to propagate *)
  | Some lit ->
      (* convert literal to (var, value) for the model *)
      (* simplify the CNF with respect to lit *)
      let simplified, assigned = simplify cnf lit in
      (* recurse with updated CNF and model *)
        unit_propagate simplified (assigned :: model)


let rec pure_literal_eliminate (cnf : cnf) (model : model) =
  match find_pure_literals cnf with
  | [] -> (cnf, model)  (* no pure literals left *)
  | lit :: _ ->
      let simplified, assigned = simplify cnf lit in
      pure_literal_eliminate simplified (assigned :: model)

let rec dpll (cnf : cnf) (model : model) : result =
  (* Unit propagation *)
  let cnf', model' = unit_propagate cnf model in

  (* Pure literal elimination *)
  let cnf'', model'' = pure_literal_eliminate cnf' model' in

  (* Check termination *)
  if cnf'' = [] then Sat model''  (* all clauses satisfied *)
  else if List.exists ((=) []) cnf'' then Unsat  (* empty clause = conflict *)
  else
    (* Pick a variable to branch *)
    let pick_var =
      match List.hd (List.hd cnf'') with
      | Var v -> v
      | Not v -> v
    in
    (* Branch: assign true first *)
    match dpll ([ [Var pick_var] ] @ cnf'') model'' with
    | Sat m -> Sat m
    | Unsat -> dpll ([ [Not pick_var] ] @ cnf'') model''