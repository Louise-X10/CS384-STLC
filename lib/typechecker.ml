open Ast

exception TypeError of string

type env = (lc_expr*typ) list

let apply_types: typ * typ -> typ = function
  | FuncTy (t1,t2), (_ as t3) ->
    if t1 == t3 then t2 
    else raise (TypeError ("Ill-typed expression, expected type" ^ typ_to_str t1 ^ "but received type" ^ typ_to_str t3))
  | (_ as t), _ -> raise (TypeError ("Ill-typed expression, expected function type but received" ^ typ_to_str t))

let rec typeof: env * lc_expr -> typ = function
  | (_ as g), (EVar _ as x) -> (
    try 
      List.assoc x g
    with 
      Not_found -> raise (TypeError ("Ill-typed expression, variable doesn't have type in environment"))
  )
  | _ as g, EApp (e1, e2) -> 
    let t1 = typeof (g, e1) in 
    let t2 = typeof (g, e2) in 
    apply_types (t1, t2)
  | _ as g, ELambda(x, t1, e) -> 
    let g' = (EVar x, t1) :: g in
    let t2 = typeof (g', e) in
    FuncTy (t1, t2)
  | _, ETrue -> BoolTy
  | _, EFalse -> BoolTy
  | _, EInt _ -> IntTy
  | _, EUnit -> UnitTy

let typecheck (e: lc_expr) : typ = 
    let g : (lc_expr*typ) list = [] in 
    typeof (g, e) 
