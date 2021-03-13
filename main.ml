

type literal = {
  lit : char;
  sign : bool;
}

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


let rec dpll clauses =
  List.iter printList clauses ; print_string "\n" ;
  if List.exists isEmpty clauses then false
  else match hasUnitClause clauses with
  | Some x -> dpll (filterMap (unitPropagate x) clauses)
  | None -> true

let _ =
  dpll [
    [
      {lit = 'A'; sign = true}
    ];
    [
      {lit = 'A'; sign = false};
      {lit = 'B'; sign = true};
      {lit = 'C'; sign = true}
    ]
  ]