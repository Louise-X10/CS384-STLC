%{
    open Ast
%}

%token Lambda
%token Dot
%token <string> Var
%token LParen
%token RParen
%token TInt
%token TBool
%token TUnit
%token <int> Int
%token True
%token False
%token Colon
%token Arrow
%token EOF

%start <lc_expr> start
%type <lc_expr> expr
%type <lc_expr> application
%type <lc_expr> base
%type <typ> typee
%type <typ> basic

/* 
<expr> ::= $id
         | <expr> <expr>
         | & $id : <type> . <expr>
         | ()
         | $int
         | true
         | false
         | ( <expr> )

<type> ::= <type> -> <type>
         | int
         | bool
         | unit
*/

/* 
<expr> ::= & $id : <type> . <expr> | <application>
<application> ::= <application> <base> | <base>
<base> ::= $id | $int | ( <expr> ) | true | false

<type> ::= <basic> -> <type> | <basic>
<basic> ::= int | bool | unit
*/


%%

start:
  | p = expr; EOF; { p }

expr:
  | Lambda; id = Var; Colon; t = typee; Dot; e = expr; { ELambda (id, t, e) }
  | a = application;                 { a }

application:
  | a = application; b = base; { EApp (a, b) }
  | b = base;                  { b }

base:
  | id = Var;                 { EVar id }
  | i = Int;                  { EInt i}
  | LParen; t = expr; RParen; { t }
  | True                      { ETrue }
  | False                     { EFalse }
  | LParen; RParen;           { EUnit }

typee:
  | t1 = basic; Arrow; t2 = typee  {FuncTy (t1, t2)}
  | t = basic;                    { t }

basic:
  | TInt                       {IntTy}
  | TBool                      {BoolTy}
  | TUnit                      {UnitTy}
