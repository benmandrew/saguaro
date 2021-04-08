
open Dpll

let printLiteral lit =
  if not lit.sign then print_string "~" ;
  print_string lit.lit ; print_string " "

let rec printListAux = function
  | [] -> print_string "}\n"
  | x::l -> printLiteral x ; printListAux l

let printList l =
  print_string "{ ";
  printListAux l

let printClauses clauses =
  List.iter (fun x -> printList x) clauses

let rec repeatChar n c =
  print_char c;
  if n > 0 then repeatChar (n-1) c

let rec printTreeAux tree level =
  let open Parser in
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

