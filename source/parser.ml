




let _ =
  let open Dpll in
  let clauses = [
    [
      {lit = 'A'; sign = false};
      {lit = 'D'; sign = false};
      {lit = 'E'; sign = true};
    ];
    [
      {lit = 'A'; sign = false};
      {lit = 'F'; sign = true};
      {lit = 'E'; sign = false};
    ];
    [
      {lit = 'A'; sign = false};
      {lit = 'F'; sign = false};
      {lit = 'G'; sign = true};
    ];
    [
      {lit = 'A'; sign = false};
      {lit = 'G'; sign = false};
      {lit = 'E'; sign = false};
    ];
    [
      {lit = 'A'; sign = true};
      {lit = 'D'; sign = true};
      {lit = 'F'; sign = true};
    ];
  ] in
  let model = dpll clauses in
  printClauses clauses;
  print_char '\n' ;
  if model == [] then print_string "No satisfying model found\n"
  else (
    print_string "Model found:\n";
    List.iter (
      fun lit ->
        if not lit.sign then print_string "¬" ;
        print_char lit.lit ; print_string " " )
      model;
    print_char '\n')
