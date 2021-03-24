
type token =
  | Literal of string
  | And
  | Or
  | Neg

let print_token = function
  | Literal s -> print_string s; print_char ' '
  | And -> print_string "/\\ "
  | Or -> print_string "\\/ "
  | Neg -> print_string "~"


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

and lexAnd input tokens =
  match input with
  | [] | _::[] -> raise (Failure "Hmm")
  | '/'::'\\'::l -> lexAux l (And::tokens)
  | _ -> raise (Failure "Hmm")

and lexOr input tokens =
  match input with
  | [] | _::[] -> raise (Failure "Hmm")
  | '\\'::'/'::l -> lexAux l (Or::tokens)
  | _ -> raise (Failure "Hmm")

and lexAux input tokens =
  match input with
  | [] -> tokens
  | c::tail -> match c with
    | ' ' -> lexAux tail tokens
    | c when isAlpha c -> lexVariable input tokens
    | '~' -> lexAux tail (Neg::tokens)
    | '/' -> lexAnd input tokens
    | '\\' -> lexOr input tokens
    | _ -> raise (Failure "Hmm")

let lex input =
  lexAux input []
  |> List.rev


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let _ =
  let input = "A \\/ ~ B" in
  print_string input;
  print_char '\n';

  print_char '[';
  lex (explode input)
  |> List.iter print_token;
  print_string "]\n\n";








