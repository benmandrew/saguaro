type token =
  | Literal of string
  | And
  | Or
  | Neg
  | Implies
  | Iff
  | LeftBracket
  | RightBracket

val lex : string -> token list
