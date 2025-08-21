open Satsolver_lib.Types

let test_parse_simple_clause_1 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a)") in
  let tree = [[Var 'a']] in
  assert (test = tree)

let test_parse_simple_clause_2 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a)") in
  let tree = [[Var 'b']] in
  assert (test != tree)

let test_parse_simple_not_1 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a)") in
  let tree = [[Not 'a']] in
  assert (test = tree)

let test_parse_simple_not_2 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a)") in
  let tree = [[Var 'a']] in
  assert (test != tree)

let test_parse_or_1 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b)") in
  let tree = [[Var 'a'; Var 'b']] in
  assert (test = tree)

let test_parse_or_2 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a | b)") in
  let tree = [[Not 'a'; Var 'b']] in
  assert (test = tree)

let test_parse_or_3 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | ~b)") in
  let tree = [[Var 'a'; Not 'b']]in
  assert (test = tree)

let test_parse_or_4 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a | ~b)") in
  let tree = [[Not 'a'; Not 'b']] in
  assert (test = tree)

let test_parse_and_1 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a) & (b)") in
  let tree = [[Var 'a'];[Var 'b']] in
  assert (test = tree)

let test_parse_and_2 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a) & (b)") in
  let tree = [[Not 'a'];[Var 'b']] in
  assert (test = tree)

let test_parse_and_3 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a) & (~b)") in
  let tree = [[Var 'a'];[Not 'b']] in
  assert (test = tree)

let test_parse_and_4 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(~a) & (~b)") in
  let tree = [[Not 'a'];[Not 'b']] in
  assert (test = tree)


let test_parse_and_5 () =
  let test = Satsolver_lib.Parser.parse (Satsolver_lib.Lexer.lexer "(a | b) & (c)") in
  let tree = [[Var 'a'; Var 'b'];[Var 'c']] in
  assert (test = tree)


let () =
  test_parse_simple_clause_1 ();
  test_parse_simple_clause_2 ();

  test_parse_simple_not_1 ();
  test_parse_simple_not_2 ();

  test_parse_or_1 ();
  test_parse_or_2 ();
  test_parse_or_3 ();
  test_parse_or_4 ();

  test_parse_and_1 ();
  test_parse_and_2 ();
  test_parse_and_3 ();
  test_parse_and_4 ();
  test_parse_and_5 ();
  print_endline "All tests passed!"
