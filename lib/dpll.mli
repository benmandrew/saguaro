type literal = { lit : string; sign : bool }

val inv : literal -> literal
val dpll : literal list list -> bool * literal list
