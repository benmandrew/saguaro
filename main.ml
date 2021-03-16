

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
  | l::_ when l.sign == lit.sign -> []
  | _::clause -> unitPropagateAux clause lit

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

let rec foldLeft f a l =
  match l with
  | [] -> a
  | h::t -> foldLeft f (f a h) t


let rec getPureLiteralsClause (first, second) clause =
  match clause with
  | [] -> (first, second)
  | l::clause when Set.mem l second ->
    getPureLiteralsClause (first, second) clause
  | l::clause when Set.mem (inv l) first ->
    getPureLiteralsClause (first, Set.add l second) clause
  | l::clause ->
    getPureLiteralsClause (Set.add l first, second) clause

let pureLiteralsAux (first, second) clauses =
  foldLeft (
    fun (first, second) x ->
      getPureLiteralsClause (first, second) x)
    (first, second)
    clauses

let pureLiterals clauses =
  match pureLiteralsAux (Set.empty, Set.empty) clauses with
  | (first, second) ->
    let second = Set.map (fun x -> {lit=x.lit; sign=(not x.sign)}) second in
    Set.filter (fun x -> not (Set.mem x second)) first

let pureLitElim pure clauses =
  foldLeft (fun clauses pureLit ->
      List.map (fun clause ->
          List.filter (fun x -> x <> pureLit) clause
      ) clauses
  ) clauses (Set.elements pure)


let rec dpll clauses =
  if List.exists isEmpty clauses then clauses
  else match hasUnitClause clauses with
  | Some x -> dpll (filterMap (unitPropagate x) clauses)
  | None ->
    let pure = pureLiterals clauses in
    match Set.elements pure with
    | [] -> clauses (* TODO SPLIT CASE *)
    | _ -> dpll (pureLitElim pure clauses)


let rec printList = function
  | [] -> print_string "\n"
  | x::l ->
    if not x.sign then print_string "¬" ;
    print_char x.lit ; print_string " " ; printList l


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
    ]
  ] in
  let clauses' = dpll clauses in
  List.iter (fun x -> printList x) clauses ;
  print_char '\n' ;
  List.iter (fun x -> printList x) clauses' ;
  print_char '\n' ;
