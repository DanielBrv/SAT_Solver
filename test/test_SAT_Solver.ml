open Satsolver_lib.Types

let test_parse_simple () =
  let test1 = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a)") in
  let tree1 = Clause(Lit(Var 'a')) in
  assert (test1 = tree1)

let test_parse_not () =
  let test2 = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a)") in
  let tree2 = Clause(Lit(Not (Var 'a'))) in
  assert (test2 = tree2)

let test_parse_or () =
  let test3 = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b)") in
  let tree3 = Clause(Or(Var 'a', Lit(Var 'b'))) in
  assert (test3 = tree3)

let test_parse_and () =
  let test4 = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b) & (c)") in
  let tree4 = And(Or(Var 'a', Lit(Var 'b')), Clause(Lit(Var 'c'))) in
  assert (test4 = tree4)

let () =
  test_parse_simple ();
  test_parse_not ();
  test_parse_or ();
  test_parse_and ();
  print_endline "All tests passed!"
