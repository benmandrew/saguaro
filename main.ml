

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

let rec unitPropagateAux clause lit =
  match clause with
  | [] -> []
  | l::clause when l.lit <> lit.lit ->
    l::(unitPropagateAux clause lit)
  | l::clause ->
    if l.sign == lit.sign then []
    else unitPropagateAux clause lit

let unitPropagate lit clause =
  match unitPropagateAux clause lit with
  | [] -> None
  | l -> Some l

let rec filterMapAux l =
  match l with
  | [] -> []
  | (Some x)::l -> x::(filterMapAux l)
  | None::l -> filterMapAux l

let filterMap f l =
  filterMapAux (List.map f l)

let rec printList = function
  | [] -> print_string "\n" ; ()
  | x::l ->
    if not x.sign then print_string "¬" ;
    print_char x.lit ; print_string " " ; printList l


let rec getPureLiteralsClause (first, second) clause =
  match clause with
  | [] -> (first, second)
  | l::clause ->
    if Set.mem (inv l) second then
      getPureLiteralsClause (first, second) clause else
    if Set.mem l first then
      (first, Set.add (inv l) second) else
    (Set.add l first, second)

let pureLiteralsAux (first, second) clauses =
  match clauses with
  | [] -> (first, second)
  | clause::clauses ->
    fold_left (
      fun (first, second) x ->
        getPureLiteralsClause (first, second) x)
      clause

let pureLiterals clauses =
  let first = Set.empty in
  let second = Set.empty in
  match pureLiteralsAux (first, second) clauses with
  | (first, second) ->
    Set.filter (fun x -> not Set.mem x second) first


let rec dpll clauses =
  List.iter printList clauses ; print_string "\n" ;
  if List.exists isEmpty clauses then false
  else match hasUnitClause clauses with
  | Some x -> dpll (filterMap (unitPropagate x) clauses)
  | None -> true

let _ =
  (* dpll [ *)
  let (first, second) = pureLiterals [
    [
      {lit = 'A'; sign = true}
    ];
    [
      {lit = 'A'; sign = false};
      {lit = 'B'; sign = true};
      {lit = 'C'; sign = true}
    ];
    [
      {lit = 'A'; sign = true};
      {lit = 'B'; sign = false};
      {lit = 'C'; sign = false}
    ]
  ] in
  List.iter (fun x -> printList x) first;
  List.iter (fun x -> printList x) second;
