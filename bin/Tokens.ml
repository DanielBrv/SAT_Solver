type token =
    | Tok_RParen
    | Tok_LParen
    | Tok_And
    | Tok_Or
    | Tok_Not
    | Tok_Eq
    | Tok_Lt
    | Tok_Gt
    | Tok_Var of char