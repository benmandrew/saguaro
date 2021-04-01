
type unaryOp =
  | Neg

type binaryOp =
  | And
  | Or
  | Implies
  | Iff

type tree =
  | Literal of string
  | True
  | False
  | Unary of unaryOp * tree
  | Binary of tree * binaryOp * tree


(* let rec printList = function
  | [] -> print_char '\n'
  | x::l -> Lexer.print_token x; printList l *)


let rec parseIff tokens =
  let tokens1, left = parseImplies tokens in
  match tokens1 with
  | Lexer.Iff::tokens2 ->
    let tokens3, right = parseIff tokens2 in
    (tokens3, Binary(left, Iff, right))
  | _ -> (tokens1, left)

and parseImplies tokens =
  let tokens1, left = parseOr tokens in
  match tokens1 with
  | Lexer.Implies::tokens2 ->
    let tokens3, right = parseImplies tokens2 in
    (tokens3, Binary(left, Implies, right))
  | _ -> (tokens1, left)

and parseOr tokens =
  let tokens1, left = parseAnd tokens in
  match tokens1 with
  | Lexer.Or::tokens2 ->
    let tokens3, right = parseOr tokens2 in
    (tokens3, Binary(left, Or, right))
  | _ -> (tokens1, left)

and parseAnd tokens =
  let tokens1, left = parseNeg tokens in
  match tokens1 with
  | Lexer.And::tokens2 ->
    let tokens3, right = parseAnd tokens2 in
    (tokens3, Binary(left, And, right))
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
    | _ -> raise (Failure "Unmatched brackets"))
  | _ -> raise (Failure "Invalid token")


let parse tokens =
  match parseIff tokens with
  | ([], tree) -> tree
  | _ -> raise (Failure "Extra tokens left after parsing")


let rec repeatChar n c =
  print_char c;
  if n > 0 then repeatChar (n-1) c

let rec printTreeAux tree level =
  print_char '\n';
  repeatChar level ' ';
  match tree with
  | Literal s -> print_string s; print_char ' '
  | True -> print_string "true "
  | False -> print_string "false "
  | Unary(op, tree) -> (match op with
    | Neg -> print_char '~';
      printTreeAux tree (level+1))
  | Binary(left, op, right) -> (match op with
    | And -> print_string "/\\ "
    | Or -> print_string "\\/ "
    | Implies -> print_string "-> "
    | Iff -> print_string "<-> ");
    printTreeAux left (level+1);
    printTreeAux right (level+1)

let printTree tree =
  printTreeAux tree 0


(* let _ =
  let tokens = Lexer.lex "(A -> B /\\ B -> A) <-> (A <-> B)" in
  printTree (parse tokens);
  print_char '\n' *)
