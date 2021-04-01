
type unaryOp = Neg
type binaryOp = And | Or | Implies | Iff
type tree = Literal of string | True | False | Unary of unaryOp * tree | Binary of tree * binaryOp * tree

val parse : Lexer.token list -> tree
val printTree : tree -> unit
