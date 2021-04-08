
open Lib

let readInput () =
  let n = Array.length Sys.argv in
  if n == 2 then Sys.argv.(1)
  else raise (Failure "Invocation must be of the form 'main.exe formula")


let _ =
  let open Dpll in
  let clauses =
    readInput ()
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
  else
    print_string "Model found:\n";
    List.iter Output.printLiteral model;
    print_char '\n'
