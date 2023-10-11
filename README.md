# Simply Typed Lambda Calculus

Simply typed lambda calculus (STLC) is built upon lambda calculus with an additional type feature. *For CS 384 Programming Language and Implementation.*

**Syntax**

An ambiguous grammar for STLC (without bindings):

```
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
```

and a disambiguated grammar:

```
<expr> ::= & $id : <type> . <expr> | <application>
<application> ::= <application> <base> | <base>
<base> ::= $id | $int | ( <expr> ) | true | false

<type> ::= <basic> -> <type> | <basic>
<basic> ::= int | bool | unit
```

**Typing rules for STLC**

```
(x, t) in G
-----------
G |- x : t

G |- e1 : t1 -> t2    G |- e2 : t1
----------------------------------
         G |- e1 e2 : t2

   G U {(x, t1)} |- e : t2       (where U is set union)
------------------------------
G |- (& x : t1 . e) : t1 -> t2

------------      ----------------      -----------------      --------------
G |- i : int      G |- true : bool      G |- false : bool      G |- () : unit
```

**Semantics**

No interpretor is implemented for this homework. 

## Implementation

To build project, run `dune build`. 

To run tests, run `dune runtests`. 

- `_lib` contains original lib files provided by instructor, ignored by dune. 
- `lib` contains my files for generating lexer with ocamllex and generating parser with Menhir. 
  - `lexer.mll` defines rules to tokenize input strings into tokens. 
  - `parser.mly` defines grammar to parse tokens into AST data types. 
  - `typechecker.ml` defines typechecking rules on Simply Typed Lambda Calculus expressions
