open Mirage

let key =
  let doc = Key.Arg.info ~doc:"Propositional formula." ["formula"] in
  Key.(create "formula" Arg.(opt string "A&B->A" doc))

let main =
  foreign
    ~keys:[Key.abstract key]
    "Unikernel.Main" job

let () =
  register "main" [main]
