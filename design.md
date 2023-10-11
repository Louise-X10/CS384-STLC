**Describe a language feature you'd like to add to OCaml.**

I would like people to use the "as" keyword in OCaml to bind multiple arguments to multiple names. Right now, people would have to group several "as" statements grouped parenthetically. For example, you need to write `(_ as g), (EVar _ as x)`. But with my new feature, people would be able to write `_, EVar as g, x`. 

The syntax would be something like this, once the parser knows it's parsing for a condition, it would look for the "as" keyword, with comma separated names before it and comma separated expressions after it. After checking their lengths match, the parser would pair the names and expressions together to form an AS token. 

```
<match branch> ::= | <condition> -> <expr>
<condition> ::= <names>* as <exprs> * | ...
<names> ::= <name>, <names> | <name>
<exprs> ::= <expr>, <exprs> | <expr>
<name> ::= $id
```

Assuming an environment model, the semantics would be something like this: writing `x1, ..., xi as n1, ..., ni` would result in each name `ni` being bound to the value that each `xi` evaluates to. This essentially updates the environment with a new set of bindings which would be used to evaluate the expression after the match condition. 

```
G|- x1 : v1     ....    G|- xi : vi
--------------------------------
G |- AS((x1, ..., xi), (n1, ..., ni)) : G U {(n1, v1), ..., (ni, vi)} |-
```

**How would you go about implementing the feature you described above?**

I would define an AS token type like `type token = ... | As` which corresponds to the string "as". I would also need to define a new AST type constructor like  `type lc_expr = ... | AS of (string * lc_expr) list`. 

I would need to modify the parser, specifically the rule for parsing `<condition>` according to the grammar I listed above. The AST type AS would receive a list of pairs (name, value). 

```
%type <lc_expr> condition
%type <lc_expr> expr
%type <lc_expr list> exprs
%type <string list> names
%type <string> name

...

condition:
  | ns = names; As ; es = exprs     {AS(List.combine ns es)}
  | ...
```

I would also need to modify the interpretor, so that interpreting an AS expression would result in updating the environment with a new set of bindings. This is assuming that the method of interpreting match branches is something along the lines of: first evaluate the condition to get an update environment, and then evaluate the match expression with the new environment. 

Overall I don't think this would be too hard to implement. 