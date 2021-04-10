
type unaryOp = Neg
type binaryOp = And | Or | Implies | Iff
type tree = Literal of string | Unary of unaryOp * tree | Binary of tree * binaryOp * tree

val parse : Lexer.token list -> tree
val negate : tree -> tree
