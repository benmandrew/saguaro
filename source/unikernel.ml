open Lwt.Infix

module Main (Time : Mirage_time.S) = struct

  let start _time = 

    (* Logs.set_reporter (Logs_fmt.reporter ()) ; *)
    
    let formula = Key_gen.formula () in

    let clauses =
      Lexer.lex formula
      |> Parser.parse
      |> Parser.negate
      |> Cnf.cnf in
    
    let (success, model) = Dpll.dpll clauses in (

    Logs.info (fun f -> f "Negated clause form:\n") ;

    Logs.info (fun f -> f "%s" ((Format.fmtClauses clauses) ^ "\n")) ;

    if not success then
      Logs.info (fun f ->
        f "Contradiction derived: formula is valid\n")
    else if model == [] then
      Logs.info (fun f ->
        f "Formula is unsatisfiable\n")
    else 
      Logs.info (fun f ->
        f "Formula is satisfiable\n") ;

    Time.sleep_ns (Duration.of_sec 1) >>= fun () ->
    Lwt.return_unit)

end
