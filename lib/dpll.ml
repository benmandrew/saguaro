

type literal = {
  lit : string;
  sign : bool;
}

module Set = Set.Make(
  struct
    let compare l1 l2 =
      match String.compare l1.lit l2.lit with
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
  let l = List.fold_left (
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

let removeComplementary clauses =
  List.filter_map
    (fun clause ->
      match List.fold_left
        (fun (s, b) l ->
          if b then (s, b)
          else if Set.mem (inv l) s then (s, true)
          else (Set.add l s, b))
        (Set.empty, false) clause with
      | (_, true) -> None
      | (_, false) -> Some clause)
  clauses

let rec dpllAux clauses assignments =
  if isEmpty clauses then (true, assignments) else
  if List.exists isEmpty clauses then (false, Set.empty) else
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
      let (_, left) = dpllAux clauses assignments in
      match Set.elements left with
      | _::_ -> (true, left)
      | [] ->
        let clauses, assignments = assign split (clauses, assignments) in
        let (_, right) = dpllAux clauses assignments in
        match Set.elements right with
        | _::_ -> (true, right)
        | [] -> (true, Set.empty)
      
let dpll clauses =
  let clauses = removeComplementary clauses in
  let (success, assignments) = dpllAux clauses Set.empty in
  (success, Set.elements assignments)

