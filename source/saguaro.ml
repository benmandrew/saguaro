
open Lib

let helpText = "Usage: saguaro.exe file

Description:
  Parses propositional formulae, converts it to clause normal form, and attempts to prove it by negation and deriving a contradiction using the DPLL method.

Syntax for input:
  Propositional symbols must be strings of alphanumeric characters and/or underscores.
  Negation: '~A'
  And:      'A & B'
  Or:       'A | B'
  Implies:  'A => B'
  Iff:      'A <=> B'
  Brackets: '(A)'
"

let readFile filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan::(!lines)
    done; String.concat "\n" (!lines)
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev !lines)

let parseInput () =
  let n = Array.length Sys.argv in
  if n == 2 then
    let input = Sys.argv.(1) in
    if String.equal input "-h" || String.equal input "--help" then
      (print_string helpText ; exit 0)
    else readFile input
  else (print_string helpText ; exit 0)


let _ =
  let open Dpll in
  let input = parseInput () in

  print_string "Input:\n" ;
  print_string input ;
  print_string "\n\n" ;

  let clauses =
    Lexer.lex input
    |> Parser.parse
    |> Parser.negate
    |> Cnf.cnf in

  let (success, model) = dpll clauses in

  print_string "Negated clause form:\n" ;

  Output.printClauses clauses ;

  print_char '\n' ;

  if not success then
    print_string "Contradiction derived: formula is valid\n"
  else if model == [] then
    print_string "Formula is unsatisfiable\n"
  else (
    print_string "Formula is satisfiable\n"
    (* let inverse = List.map inv model in
    List.iter Output.printLiteral inverse;
    print_char '\n') *) )
