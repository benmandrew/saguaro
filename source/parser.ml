
type unary =
  | Neg

type binary =
  | And
  | Or
  | Implies
  | Iff

type tree =
  | Literal of string
  | True
  | False
  | Unary of unary * tree
  | Binary of tree * binary * tree

let rec parseIff tokens =
  let tokens1, left = parseImplies tokens in
  match tokens1 with
  | Lexer.Iff::tokens2 ->
    let tokens3, right = parseIff tokens2 in
    (tokens3, Binary (left, Iff, right))
  | _ -> (tokens1, left)

and parseImplies tokens =
  let tokens1, left = parseOr tokens in
  match tokens1 with
  | Lexer.Implies::tokens2 ->
    let tokens3, right = parseImplies tokens2 in
    (tokens3, Binary (left, Implies, right))
  | _ -> (tokens1, left)

and parseOr tokens =
  let tokens1, left = parseAnd tokens in
  match tokens1 with
  | Lexer.Implies::tokens2 ->
    let tokens3, right = parseOr tokens2 in
    (tokens3, Binary (left, Or, right))
  | _ -> (tokens1, left)

and parseAnd tokens =
  let tokens1, left = parseNeg tokens in
  match tokens1 with
  | Lexer.Implies::tokens2 ->
    let tokens3, right = parseAnd tokens2 in
    (tokens3, Binary (left, And, right))
  | _ -> (tokens1, left)

and parseNeg tokens =
  match tokens with
  | Lexer.Neg::tokens1 ->
    let tokens2, tree = parseNeg tokens1 in
    (tokens2, Unary (Neg, tree))
  | _ -> parseVar tokens

and parseVar tokens =
  match tokens with
  | (Lexer.Literal str)::tokens1 -> (tokens1, Literal str)
  | Lexer.True::tokens1 -> (tokens1, True)
  | Lexer.False::tokens1 -> (tokens1, False)
  | Lexer.LeftBracket::tokens1 -> (
    let tokens2, tree = parseIff tokens1 in
    match tokens2 with
    | Lexer.RightBracket::tokens3 -> (tokens3, tree)
    | _ -> raise (Failure "Hmm"))
  | _ -> raise (Failure "Hmm")

let parse tokens =
  match parseIff tokens with
  | ([], tree) -> tree
  | _ -> raise (Failure "Hmm")


















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
