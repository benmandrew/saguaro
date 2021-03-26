

type token = Literal of string | And | Or | Neg | Implies | Iff
val print_token : token -> unit
val lex : string -> token list

