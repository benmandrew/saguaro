
open Parser

let rec collect = function
  | Literal s -> [[{Dpll.lit=s; Dpll.sign=true}]]
  | Unary(Neg, Literal s) -> [[{Dpll.lit=s; Dpll.sign=false}]]
  | Binary(a, And, b) -> List.append (collect a) (collect b)
  | Binary(a, Or, b) -> (
    let a' = collect a in
    let b' = collect b in
    List.fold_left (fun cs cl ->
      List.append (List.map (fun cr ->
        List.append cl cr) b') cs) [] a')
  | _ -> raise (Failure "collection failure")

let nnfAux = function
  (* A => B ---> ~A | B *)
  | Binary(a, Implies, b) ->
    Binary(Unary(Neg, a), Or, b)
  (* A <=> B ---> (A & B) | (~A & ~B) *)
  | Binary(a, Iff, b) ->
    Binary(Binary(a, And, b), Or,
      Binary(Unary(Neg, a), And, Unary(Neg, b)))
  (* ~~A ---> A *)
  | Unary(Neg, Unary(Neg, a)) -> a
  (* ~(A & B) ---> ~A | ~B *)
  | Unary(Neg, Binary(a, And, b)) ->
    Binary(Unary(Neg, a), Or, Unary(Neg, b))
  (* ~(A | B) ---> ~A & ~B *)
  | Unary(Neg, Binary(a, Or, b)) ->
    Binary(Unary(Neg, a), And, Unary(Neg, b))
  | a -> a

let rec nnf tree = match nnfAux tree with
  | Unary(op, a) -> Unary(op, nnf a)
  | Binary(a, op, b) -> Binary(nnf a, op, nnf b)
  | leaf -> leaf

let cnf tree =
  nnf tree
  |> collect

