

type token =
  | Literal of string
  | And
  | Or
  | Neg
  | Implies
  | Iff

let print_token = function
  | Literal s -> print_string s; print_char ' '
  | And -> print_string "/\\ "
  | Or -> print_string "\\/ "
  | Neg -> print_string "~"
  | Implies -> print_string "-> "
  | Iff -> print_string "<-> "

let isAlpha c =
  let code = Char.code c in
  (code >= 65 && code <= 90) ||
  (code >= 97 && code <= 122)

let rec takeAux n l acc =
  match l with
  | [] -> (List.rev acc, [])
  | x::l when n > 0 ->
    takeAux (n-1) l (x::acc)
  | l -> (List.rev acc, l)

let take n l =
  takeAux n l []

let stringOfChars n chars =
  let buf = Buffer.create n in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let rec predExtentAux pred l n =
  match l with
  | [] -> n
  | x::l when pred x ->
    predExtentAux pred l (n+1)
  | _::_ -> n

let predExtent pred l =
  predExtentAux pred l 0

let rec lexVariable input tokens =
  let n = predExtent isAlpha input in
  let tokenChrs, tail = take n input in
  let lit = Literal (stringOfChars n tokenChrs) in
  lexAux tail (lit::tokens)

and lexAux input tokens =
  match input with
  | [] -> tokens
  | ' '::tail -> lexAux tail tokens
  | c::_ when isAlpha c -> lexVariable input tokens
  | '~'::tail -> lexAux tail (Neg::tokens)
  | '/'::'\\'::tail -> lexAux tail (And::tokens)
  | '\\'::'/'::tail -> lexAux tail (Or::tokens)
  | '-'::'>'::tail -> lexAux tail (Implies::tokens)
  | '<'::'-'::'>'::tail -> lexAux tail (Iff::tokens)
  | _ -> raise (Failure "Hmm")

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let lex input =
  lexAux (explode input) []
  |> List.rev

  
(* let _ =
  let open Lexer in
  let input = "A \\/ ~ B -> C /\\ D" in
  print_string input;
  print_char '\n';

  print_char '[';
  lex input
  |> List.iter print_token;
  print_string "]\n\n"; *)








