
open Lib

let helpText = "Usage: dpllsat.exe file

Description:
  Parses propositional formulae, converts it to clause normal form, and attempts to prove it by negation and deriving a contradiction using the DPLL method.

Syntax for input:
  Propositional symbols must be strings of alphanumeric characters and/or underscores.
  Logical true and false are 'true' and 'false'.
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
  let clauses =
    parseInput ()
    |> Lexer.lex
    |> Parser.parse
    |> Cnf.cnf in
  let (success, model) = dpll clauses in
  Output.printClauses clauses;
  print_char '\n' ;
  if not success then
    print_string "No satisfying model found\n"
  else if model == [] then
    print_string "Formula is a tautology\n"
  else (
    print_string "Model found:\n";
    List.iter Output.printLiteral model;
    print_char '\n')
