(* open Lwt.Infix *)

module Main = struct

  let start = 
    
    let formula = Key_gen.formula () in

    let clauses =
      Lexer.lex formula
      |> Parser.parse
      |> Parser.negate
      |> Cnf.cnf in
    
    let (success, model) = Dpll.dpll clauses in (

    Logs.info (fun f -> f "%s" "Negated clause form:\n") ;

    Logs.info (fun f -> f "%s" ((Format.fmtClauses clauses) ^ "\n")) ;

    if not success then
      Logs.info (fun f ->
        f "%s" "Contradiction derived: formula is valid\n")
    else if model == [] then
      Logs.info (fun f ->
        f "%s" "Formula is unsatisfiable\n")
    else 
      Logs.info (fun f ->
        f "%s" "Formula is satisfiable\n") ;

    Lwt.return_unit)

end
