

type literal = {
  lit : char;
  sign : bool;
}

module Set = Set.Make(
  struct
    let compare l1 l2 =
      match Char.compare l1.lit l2.lit with
      | 0 ->
        if l1.sign && not l2.sign then 1
        else if not l1.sign && l2.sign then -1
        else 0
      | v -> v
    type t = literal
  end)

let inv l =
  {lit=l.lit; sign=not l.sign}

let isEmpty clause =
  match clause with
  | [] -> true
  | _::_ -> false

let isUnit clause =
  match clause with
  | [] -> None
  | x::[] -> Some x
  | _::_ -> None

let rec hasUnitClause clauses =
  match clauses with
  | [] -> None
  | clause::clauses ->
    match isUnit clause with
    | None -> hasUnitClause clauses
    | Some x -> Some x

let rec unitPropagate lit clause =
  match clause with
  | [] -> []
  | l::clause when l.lit <> lit.lit ->
    l::(unitPropagate lit clause)
  | l::_ when l.sign == lit.sign -> []
  | _::clause -> unitPropagate lit clause

let rec foldLeft f a l =
  match l with
  | [] -> a
  | h::t -> foldLeft f (f a h) t

let rec isConsistentClause (occurrences, seen) clause =
  match clause with
  | [] -> (occurrences, seen)
  | l::_ when Set.mem (inv l) occurrences ->
    (occurrences, true)
  | l::clause ->
    isConsistentClause (Set.add l occurrences, seen) clause


let isConsistent clauses =
  let l = foldLeft (
    fun (occurrences, seen) x ->
      if seen then (occurrences, seen) else
      isConsistentClause (occurrences, seen) x)
    (Set.empty, false)
    clauses in
  match l with
  | (_, false) -> Set.empty
  | (occurrences, true) -> occurrences
  

let rec getPureLiteralsClause (first, second) clause =
  match clause with
  | [] -> (first, second)
  | l::clause when Set.mem l second ->
    getPureLiteralsClause (first, second) clause
  | l::clause when Set.mem (inv l) first ->
    getPureLiteralsClause (first, Set.add l second) clause
  | l::clause ->
    getPureLiteralsClause (Set.add l first, second) clause

let pureLiterals clauses =
  let l = foldLeft (
    fun (first, second) x ->
      getPureLiteralsClause (first, second) x)
    (Set.empty, Set.empty)
    clauses in
  match l with
  | (first, second) ->
    let second = Set.map (fun x -> {lit=x.lit; sign=(not x.sign)}) second in
    Set.filter (fun x -> not (Set.mem x second)) first

let pureLitElim pure clauses =
  foldLeft (fun clauses pureLit ->
      List.map (fun clause ->
          List.filter (fun x -> x <> pureLit) clause
      ) clauses
  ) clauses (Set.elements pure)


let printLiteral lit =
  if not lit.sign then print_string "¬" ;
  print_char lit.lit ; print_string " "

let rec printListAux = function
  | [] -> print_string "}\n"
  | x::l -> printLiteral x ; printListAux l

let printList l =
  print_string "{ ";
  printListAux l

let printClauses clauses =
  List.iter (fun x -> printList x) clauses

let rec dpllAux assignments clauses =
  printClauses clauses;
  if isEmpty clauses then
    (print_string "No clauses:\n"; printClauses clauses; print_char '\n';
    Set.empty) else
  if List.exists isEmpty clauses then
    (print_string "Empty clause:\n"; printClauses clauses; print_char '\n';
    assignments) else
  match hasUnitClause clauses with
  | Some x ->
    print_string "Unit clause:\n"; printClauses clauses; print_char '\n';
    dpllAux (Set.add x assignments) (List.map (unitPropagate x) clauses)
  | None -> (
    let pure = pureLiterals clauses in
    match Set.elements pure with
    | _::_ -> (
      print_string "Pure literals:\n"; printClauses clauses; print_char '\n';
      dpllAux (Set.union assignments pure) (pureLitElim pure clauses))
    | [] -> (
      let split = List.hd (List.hd clauses) in
      print_string "Split left:\n"; printLiteral split ; print_char '\n' ; printClauses clauses; print_char '\n';
      let left = dpllAux (Set.add split assignments) (List.map (unitPropagate split) clauses) in
      match Set.elements left with
      | _::_ -> left
      | [] -> (
        print_string "Split right:\n"; printClauses clauses; print_char '\n';
        let right = dpllAux (Set.add (inv split) assignments) (List.map (unitPropagate (inv split)) clauses) in
        match Set.elements right with
        | _::_ -> right
        | [] -> Set.empty)))

let dpll clauses =
  Set.elements (dpllAux Set.empty clauses)

let _ =
  let clauses = [
    (* [
      {lit = 'A'; sign = true};
    ]; *)
    [
      {lit = 'A'; sign = false};
      {lit = 'B'; sign = true};
      {lit = 'C'; sign = false};
    ];
    [
      {lit = 'A'; sign = true};
      {lit = 'B'; sign = false};
      {lit = 'D'; sign = true};
    ];
  ] in
  let model = dpll clauses in
  (* printClauses clauses;
  print_char '\n' ; *)
  if model == [] then print_string "No satisfying model found\n"
  else (
    print_string "Model found:\n";
    List.iter (
      fun lit ->
        if not lit.sign then print_string "¬" ;
        print_char lit.lit ; print_string " " )
      model;
    print_char '\n')




