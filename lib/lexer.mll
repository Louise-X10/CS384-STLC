{

open Parser

exception SyntaxError of string

}

let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let int = ['0'-'9']*
let whitespace = [' ' '\t' '\n' '\r']+

rule tok = parse
| whitespace { tok lexbuf}
| '&'        { Lambda }
| '.'        { Dot }
| '('        { LParen }
| ')'        { RParen }
| "int"      { TInt }
| "bool"     { TBool }
| "unit"     { TUnit }
| "true"     { True }
| "false"    { False }
| int as i   { Int (int_of_string i) }
| id as s    { Var s }
| ':'        { Colon}
| "->"       { Arrow}
| eof        { EOF }
| _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

{

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
  | EOF -> "<eof>"

let tokenize (s : string) : token list =
  let buf = Lexing.from_string s in
  let rec helper acc =
    match tok buf with
    | EOF -> List.rev acc
    | t -> helper (t :: acc) in
  helper []

let parse_lexbuf (b : Lexing.lexbuf) : Ast.lc_expr =
  start tok b

let parse s = parse_lexbuf (Lexing.from_string s)

}