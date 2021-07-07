open Mirage

let key =
  let doc = Key.Arg.info ~doc:"Propositional formula." ["formula"] in
  Key.(create "formula" Arg.(opt string "A&B->A" doc))

(* ~keys:[Key.abstract key] *)

let main =
  foreign
    ~keys:[Key.abstract key]
    ~packages:[package "duration"]
    "Unikernel.Main" (time @-> job)

let () =
  register "main" [main $ default_time]
