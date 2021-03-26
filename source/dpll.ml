

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

let rec foldLeft f a l =
  match l with
  | [] -> a
  | h::t -> foldLeft f (f a h) t

(* let rec isConsistentClause (occurrences, seen) clause =
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
  | (occurrences, true) -> occurrences *)

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

let assign lit (clauses, assignments) =
  let clauses' = List.map (fun clause ->
    if List.mem lit clause then None
    else Some (List.filter (fun el -> el <> (inv lit)) clause))
    clauses
  |> List.filter (fun x -> x <> None)
  |> List.map (fun x -> match x with
    | None -> raise (Failure "List contains None after filtering")
    | Some x -> x) in
  (clauses', Set.add lit assignments)

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

let rec dpllAux clauses assignments =
  if isEmpty clauses then assignments else
  if List.exists isEmpty clauses then Set.empty else
  match hasUnitClause clauses with
  | Some x ->
    let (clauses, assignments) = assign x (clauses, assignments) in
    dpllAux clauses assignments
  | None ->
    let pure = pureLiterals clauses in
    match Set.elements pure with
    | _::_ ->
      let clauses, assignments = Set.fold assign pure (clauses, assignments) in
      dpllAux clauses assignments
    | [] ->
      let split = List.hd (List.hd clauses) in
      let clauses, assignments = assign split (clauses, assignments) in
      let left = dpllAux clauses assignments in
      match Set.elements left with
      | _::_ -> left
      | [] ->
        let clauses, assignments = assign split (clauses, assignments) in
        let right = dpllAux clauses assignments in
        match Set.elements right with
        | _::_ -> right
        | [] -> Set.empty

let dpll clauses =
  Set.elements (dpllAux clauses Set.empty)


(* let _ =
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
    print_char '\n') *)




