

type literal = { lit : char; sign : bool; }
val inv : literal -> literal
val printLiteral : literal -> unit
val printList : literal list -> unit
val printClauses : literal list list -> unit
val dpll : literal list list -> literal list

