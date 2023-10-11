exception LexError of string

type token =
  | Lambda
  | Dot
  | Var of string
  | LParen
  | RParen
  | TInt
  | TBool
  | TUnit
  | Int of int
  | True
  | False
  | Colon
  | Arrow
  | Equal
  | Semicolon

let tok_to_str : token -> string = function
  | Lambda -> "&"
  | Dot -> "."
  | Var i -> i
  | LParen -> "("
  | RParen -> ")"
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | Int i -> string_of_int i
  | True -> "true"
  | False -> "false"
  | Colon -> ":"
  | Arrow -> "->"
  | Equal -> "="
  | Semicolon -> ";"

let is_digit (c : char) : bool = '0' <= c && c <= '9'

let is_id_char (c : char) : bool =
  c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || is_digit c

let consume_int (i : int) (src : string) : int * token =
  let rec loop j acc =
    if j >= String.length src || not (is_digit src.[j])
    then (j, Int (int_of_string acc))
    else loop (j + 1) (acc ^ String.make 1 src.[j]) in
  loop i ""

let consume_id (i : int) (src : string) : int * token =
  let rec loop j acc =
    if j >= String.length src || not (is_id_char src.[j])
    then (j, match acc with
      | "int" -> TInt
      | "bool" -> TBool
      | "unit" -> TUnit
      | "true" -> True
      | "false" -> False
      | s -> Var s)
    else loop (j + 1) (acc ^ String.make 1 src.[j]) in
  loop i ""

let tokenize (src : string) : token list =
  let rec loop i acc =
    if i >= String.length src
    then List.rev acc
    else match src.[i] with
      | '&' -> loop (i + 1) (Lambda :: acc)
      | '.' -> loop (i + 1) (Dot :: acc)
      | '(' -> loop (i + 1) (LParen :: acc)
      | ')' -> loop (i + 1) (RParen :: acc)
      | ':' -> loop (i + 1) (Colon :: acc)
      | '=' -> loop (i + 1) (Equal :: acc)
      | ';' -> loop (i + 1) (Semicolon :: acc)
      | '-' ->
        if src.[i+1] = '>'
        then loop (i + 2) (Arrow :: acc)
        else raise (LexError ("Expected >, got: " ^ String.make 1 src.[i+1]))
      | c when is_digit c ->
        let (ni, tok) = consume_int i src in
        loop ni (tok :: acc)
      | c when c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ->
        let (ni, tok) = consume_id i src in
        loop ni (tok :: acc)
      | c when String.contains "\n\r\t " c -> loop (i + 1) acc
      | c -> raise (LexError ("Unexpected character: " ^ String.make 1 c))
  in
  loop 0 []
