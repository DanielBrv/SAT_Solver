
open Satsolver_lib.Types
open Lwt.Infix
open Cohttp_lwt_unix

(*
let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []
;;
*)
(* returns string representation of var *)
let print_var var = "x" ^ string_of_int var

(* prints out *)
let print_lit tree =
  match tree with
  | Not(a) -> "¬" ^ print_var a 
  | Var(a)-> print_var a 

let rec print_clause clause =
  match clause with
  | lit :: [] -> print_lit lit
  | lit :: t -> print_lit lit  ^ " ∨ " ^ print_clause t
  | [] -> ""


let rec print_cnf (cnf: cnf) =
  match cnf with
  | clause :: [] -> "(" ^ print_clause clause ^ ")"
  | clause :: t -> "(" ^ print_clause clause ^ ")" ^ " ∧ " ^ print_cnf t
  | [] -> "()"

let rec print_model (model : model) : string =
  match model with
  | (v,b) :: [] -> "( "^ print_var v ^ " -> " ^ string_of_bool b ^" )"
  | (v,b) :: rest -> "( "^ print_var v ^ " -> " ^ string_of_bool b ^ " ), " ^ print_model rest
  | [] -> ""

let print_result (result : result) =
  match result with
  | Sat(model) ->  "Sat" ^ print_model model
  | Unsat ->  "Unsat"    
(*

   curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{"vars": 4, "clauses": 3, "cnf": [[1, -3], [2, 3, -1], [4]]}'

  *)
let callback _conn req body =
  let uri = Request.uri req in
  match Request.meth req with
  | `POST ->
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let response =
        try
          let json = Yojson.Safe.from_string body_str in
          let _ = (* num_vars *)
            json |> Yojson.Safe.Util.member "vars" |> Yojson.Safe.Util.to_int
          in
          let num_clauses = 
            json |> Yojson.Safe.Util.member "clauses" |> Yojson.Safe.Util.to_int
          in
          let cnf =
            json
            |> Yojson.Safe.Util.member "cnf"
            |> Yojson.Safe.Util.to_list
            |> List.map (fun clause ->
                 clause
                 |> Yojson.Safe.Util.to_list
                 |> List.map Yojson.Safe.Util.to_int)
          in
          let cnfs = Satsolver_lib.Parser.parse_clauses cnf num_clauses in 
            let _ = print_cnf cnfs in print_result (Satsolver_lib.Solver.dpll cnfs [])
            
        with
        | Yojson.Json_error msg -> "Invalid JSON: " ^ msg
        | Yojson.Safe.Util.Type_error (msg, _) -> "Type error: " ^ msg ^ "\n"
        | _ -> "Error parsing JSON\n"
      in
      Server.respond_string ~status:`OK ~body:response ()
  | `GET ->
      let path = Uri.path uri in
      Server.respond_string ~status:`OK ~body:("GET request for " ^ path ^ "\n") ()
  | _ ->
      Server.respond_string ~status:`Method_not_allowed
        ~body:"Only GET and POST allowed\n" ()

let start_server () =
  let mode = `TCP (`Port 8080) in
  let config = Server.make ~callback () in
  Server.create ~mode config

let () = Lwt_main.run (start_server ())
(* 
   Conjunctive normal form
  CNF       → Disjunct
           | Disjunct ∧ CNF

  Disjunct  → Literal
           | Literal ∨ Disjunct

  Literal   → Variable
           | ¬ Literal
*)
(* Takes in filename and returns a list containing a strings for each line of the file *)

          
(*
let dimacs = read_lines "cnf.txt"

let print_lines lines =
  List.iter print_endline lines
;;

let () = print_lines dimacs
let () = print_endline ("CNF: "^ print_cnf cnf  )
*)






