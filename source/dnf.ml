
open Parser


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


(* let collectClauses = function
| Unary(Neg, a) -> Unary(op, nnf a)
| Unary(Neg, True) -> False
| Unary(Neg, False) -> True
| Binary(a, op, b) -> Binary(nnf a, op, nnf b)
| leaf -> leaf *)



let _ =
  let tree =
    "~(((~A | ~B) & (~C | ~D)) | ((~E | ~F) & (~G | ~H)))"
    |> Lexer.lex
    |> parse in

  (* printTree tree; *)

  printTree (nnf tree);
  print_char '\n'



