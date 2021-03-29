

type token = Literal of string | True | False | And | Or | Neg | Implies | Iff | LeftBracket | RightBracket
val print_token : token -> unit
val lex : string -> token list

