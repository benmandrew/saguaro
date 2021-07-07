
open Dpll

let fmtLiteral lit =
  if not lit.sign then "~" ^ lit.lit ^ " "
  else lit.lit ^ " "

let rec fmtListAux = function
  | [] -> "}\n"
  | x::l -> (fmtLiteral x) ^ (fmtListAux l)

let fmtList l =
  "{ " ^ fmtListAux l

let fmtClauses clauses =
  List.fold_left (fun s -> fun x -> s ^ (fmtList x)) "" clauses

let fmtToken t =
  let open Lexer in
  match t with
  | Literal s -> s ^ " "
  | And -> "& "
  | Or -> "| "
  | Neg -> "~"
  | Implies -> "=> "
  | Iff -> "<=> "
  | LeftBracket -> "( "
  | RightBracket -> ") "

let rec repeatStr n s =
  s ^ (
    if n > 0 then repeatStr (n-1) s
    else "")

let rec fmtTreeAux tree level =
  let open Parser in
  "\n" ^ (repeatStr level "") ^ (
    match tree with
    | Literal s -> s ^ " "
    | Unary(op, tree) -> (match op with
      | Neg -> "~" ^ (fmtTreeAux tree (level+1)))
    | Binary(left, op, right) -> (match op with
      | And -> "/\\ "
      | Or -> "\\/ "
      | Implies -> "=> "
      | Iff -> "<=> ") ^
      (fmtTreeAux left (level+1)) ^
      (fmtTreeAux right (level+1)))

let fmtTree tree =
  fmtTreeAux tree 0 ^ "\n"

