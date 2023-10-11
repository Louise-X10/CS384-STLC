open OUnit2
open Lambda_calculus.Lexer
open Lambda_calculus.Ast
open Lambda_calculus.Parser
open Lambda_calculus.Typechecker

let lex_tests = "test suite for lexer" >::: [
    "all tokens" >::
    (fun _ -> assert_equal ~printer:(fun l -> String.concat " " (List.map tok_to_str l))
        [Lambda; Dot; Var "x"; LParen; RParen; Colon; Arrow]
        (tokenize "&.x():->"));
    "numbers in id" >::
    (fun _ -> assert_equal [Var "_32x"] (tokenize "_32x"));
  ]

let equiv_tests = "test suite for alpha-equivalence" >::: [
    "variable" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "x")
           (parse "y")));
    "lambda" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "&x.x")
           (parse "&y.y")));
    "lambda unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "&x.x")
           (parse "&y.x"))));
    "application" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&x.x) a")
           (parse "(&y.y) a")));
    "application right argument" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&x.x) (&y.y)")
           (parse "(&y.y) (&z.z)")));
    "application right argument unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "(&x.x) (&y.y)")
           (parse "(&y.y) (&z.a)"))));
    "nested lambda" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (alpha_equiv
           (parse "(&f. &x. f x)")
           (parse "(&s. &z. s z)")));
    "nested lambda unequal" >::
    (fun _ -> assert_bool "alpha-equivalence check failed"
        (not (alpha_equiv
           (parse "(&f. &x. f f)")
           (parse "(&s. &z. s z)"))));
  ]

let parse_tests = "test suite for parser" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", IntTy, (EVar "x")))
        (parse "&x: int.x"));
    "application" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("x", IntTy, (EVar "x")), EVar "y"))
        (parse "(&x: int .x) y"));
    "apply inside lambda" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", IntTy, EApp (EVar "x", EVar "y")))
        (parse "&x: int .x y"));
    "application convoluted" >::
        (fun _ -> assert_equal ~printer:print_lc_expr
            (ELambda ("x", FuncTy(IntTy,IntTy), 
                ELambda("y", IntTy, EApp(EVar "x", EApp (EVar "x", EVar "y")))))            
            (parse "& x : int -> int . & y : int . x (x y)"));
        
   (*  "naming" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp (ELambda ("fun", (EApp (EVar "fun", EVar "v"))),
               ELambda ("x", ELambda ("y", EVar "x"))))
        (parse "fun = &x. &y. x; fun v")); 
    "naming church numerals" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EApp
           (ELambda
              ("zero",
               EApp
                 (ELambda
                   ("succ",
                    EApp (EVar "succ", EApp (EVar "succ", EVar "zero"))),
                 (ELambda
                    ("n",
                     ELambda
                       ("f",
                        ELambda
                          ("x",
                           EApp (EVar "f",
                                 EApp (EApp (EVar "n", EVar "f"),
                                       EVar "x")))))))),
         (ELambda ("f", ELambda ("x", EVar "x")))))
        (parse ("zero = &f. &x. x;" ^
                "succ = &n. &f. &x. f (n f x);" ^
                "succ (succ zero)")));*)
  ]

(* let reduce_tests = "test suite for the reduction engine" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (EVar "y")
        (reduce (parse "(&x.x)y")));
    "two" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (parse "&f. &x. f (f x)")
        (reduce (parse ("zero = &f. &x. x;" ^
                        "succ = &n. &f. &x. f (n f x);" ^
                        "succ (succ zero)"))));
    "shadowing" >::
    (fun _ -> assert_equal ~printer:print_lc_expr
        (ELambda ("x", EVar "x"))
        (reduce (parse "(&x. &x. x) y")));
  ] *)

let type_tests = "test suite for typechecker" >::: [
    "id" >::
    (fun _ -> assert_equal ~printer:typ_to_str
        (FuncTy (IntTy, IntTy))
        (typecheck (parse"&x: int.x")));
    "application" >::
    (fun _ -> assert_equal ~printer:typ_to_str
        (IntTy)
        (typecheck (parse "(&x: int .x) 5")));
    "application convoluted" >::
        (fun _ -> assert_equal ~printer:typ_to_str
            (FuncTy( FuncTy(IntTy, IntTy), FuncTy(IntTy, IntTy) ))
            (typecheck (parse "& x : int -> int . & y : int . x (x y)")));
    "unit type" >::
        (fun _ -> assert_equal ~printer:typ_to_str
            (UnitTy)
            (typecheck (parse "(& x : int . & y : bool . ()) 2 false")));
    "ill-typed" >::
        (fun _ -> try
            let _ = typecheck (parse "(& x : int . & y : bool . ()) 3 ()") in
            assert_failure "'&x: int .x y' passed the typechecker"
        with
        | TypeError _ -> assert_bool "" true
        | _ -> assert_failure "Unexpected error");
    "apply inside lambda" >::
        (fun _ -> try
            let _ = typecheck (parse "&x: int .x y") in
            assert_failure "'&x: int .x y' passed the typechecker"
        with
        | TypeError _ -> assert_bool "" true
        | _ -> assert_failure "Unexpected error");
  ]


let tests = "test_suite for lambda calculus" >::: [
    lex_tests;
    (* equiv_tests; *)
    parse_tests;
    type_tests;
    (* reduce_tests; *)
  ]

let _ = run_test_tt_main tests
