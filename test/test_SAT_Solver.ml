open Satsolver_lib.Types
let () = print_endline "Test"


let test1 = (Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a)" ))
let tree1 = Clause(Lit(Var('a')))

let test2 = (Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a)" ))
let tree2 = Clause(Lit(Not(Var('a'))))

let test3 = (Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b)" ))
let tree3 = Clause( Or(Var('a'), Lit(Var('b'))))

let test4 = (Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b) & (c)" ))
let tree4 = And(Or(Var('a'), Lit(Var('b'))), Clause(Lit(Var('c'))))

let () =
  assert (test1 = tree1);
  assert (test2 = tree2);
  assert (test3 = tree3);
  assert (test4 = tree4);

  print_endline "All tests passed!"
