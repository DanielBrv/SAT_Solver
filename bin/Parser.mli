val parse_literal : Tokens.token list -> Types.lit * Tokens.token list
val parse_disjunct : Tokens.token list -> Types.disjunct * 'a list
val parse_cnf : Tokens.token list -> Types.cnf * 'a list
val parse : Tokens.token list -> Types.cnf
