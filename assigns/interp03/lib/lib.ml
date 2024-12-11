open Utils
include My_parser

let rec occurs x ty =
  match ty with
  | TVar y -> x = y
  | TFun (t1, t2) | TPair (t1, t2) -> occurs x t1 || occurs x t2
  | TList t | TOption t -> occurs x t
  | _ -> false


let rec frees ty =
  match ty with
  | TVar x -> [x]
  | TFun (t1, t2) | TPair (t1, t2) -> frees t1 @ frees t2
  | TList t | TOption t -> frees t
  | _ -> []

let rec substitute subst ty =
  match ty with
  | TVar x -> (try List.assoc x subst with Not_found -> ty)
  | TFun (t1, t2) -> TFun (substitute subst t1, substitute subst t2)
  | TPair (t1, t2) -> TPair (substitute subst t1, substitute subst t2)
  | TList t -> TList (substitute subst t)
  | TOption t -> TOption (substitute subst t)
  | _ -> ty

let new_sort cmp lst =
  let sorted = List.sort cmp lst in
  let rec uniq acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: (y :: _ as rest) -> if cmp x y = 0 then uniq acc rest else uniq (x :: acc) rest
  in
  uniq [] sorted

let rec unify ty constraints =
  match constraints with
  | [] -> 
    let free = new_sort compare (frees ty) in
    Some (Forall (free, ty)) 
  | (t1, t2) :: rest when t1 = t2 -> unify ty rest 
  | (TVar x, t) :: rest | (t, TVar x) :: rest ->
    if occurs x t then None 
    else
      let subst = [(x, t)] in
      let unified_ty = substitute subst ty in
      let unified_consts = 
      List.map (fun (t1, t2) -> (substitute subst t1, substitute subst t2)) rest in
      (match unify unified_ty unified_consts with
        | Some (Forall (vars, final_ty)) ->
          let uniq_vars = List.filter (fun v -> v <> x) vars in
          Some (Forall (uniq_vars, final_ty))
        | None -> None)
  | (TFun (t1_1, t1_2), TFun (t2_1, t2_2)) :: rest ->
    unify ty ((t1_1, t2_1) :: (t1_2, t2_2) :: rest)
  | (TPair (t1_1, t1_2), TPair (t2_1, t2_2)) :: rest ->
    unify ty ((t1_1, t2_1) :: (t1_2, t2_2) :: rest)
  | (TList t1, TList t2) :: rest | (TOption t1, TOption t2) :: rest ->
    unify ty ((t1, t2) :: rest)
  | (TInt, TFloat) :: _ | (TFloat, TInt) :: _ -> None 
  | (TBool, TInt) :: _ | (TBool, TFloat) :: _ -> None 
  | _ -> None 


let instantiate (vars, ty) =
  let subst = List.map (fun var -> (var, TVar (gensym ()))) vars in
  substitute subst ty

let type_of env exp = 
  let rec loop env = function
  | Unit -> (TUnit, [])
  | True | False -> (TBool, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Var x -> (
      match Env.find_opt x env with
      | Some (Forall (vars, t)) -> (instantiate (vars, t), [])
      | None -> failwith ("Unbound variable")
    )
  | ENone -> (TOption (TVar (gensym ())), [])
  | ESome e ->
    let t, c = loop env e in
    (TOption t, c)
  | Nil -> (TList (TVar (gensym ())), [])
  | OptMatch { matched; some_name; some_case; none_case } ->
    let fresh = TVar (gensym ()) in
    let t_matched, c_matched = loop env matched in
    let env_with_some = Env.add some_name (Forall ([], fresh)) env in
    let t_some_case, c_some = loop env_with_some some_case in
    let t_none_case, c_none = loop env none_case in
    let constraints =
      (t_matched, TOption fresh) ::
      (t_some_case, t_none_case) :: 
      c_matched @ c_some @ c_none
    in
    (t_some_case, constraints)
  | Bop (op, e1, e2) -> (
      let t1, c1 = loop env e1 in
      let t2, c2 = loop env e2 in
      match op with
      | Add | Sub | Mul | Div | Mod ->
          (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
      | AddF | SubF | MulF | DivF | PowF ->
          (TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2)
      | And | Or ->
          (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
      | Eq | Neq | Lt | Lte | Gt | Gte ->
          let fresh = TVar (gensym ()) in
          (TBool, (t1, fresh) :: (t2, fresh) :: c1 @ c2)
      | Cons ->
        let t1, c1 = loop env e1 in
        let t2, c2 = loop env e2 in
        (TList t1, (t2, TList t1) :: c1 @ c2)
      | Concat ->
        let t1, c1 = loop env e1 in
        let t2, c2 = loop env e2 in
        let fresh = TVar (gensym ()) in
        (TList fresh, (t1, TList fresh) :: (t2, TList fresh) :: c1 @ c2)
      | Comma ->
        let t1, c1 = loop env e1 in
        let t2, c2 = loop env e2 in
        (TPair (t1, t2), c1 @ c2)
    )
  | If (e1, e2, e3) ->
      let t1, c1 = loop env e1 in
      let t2, c2 = loop env e2 in
      let t3, c3 = loop env e3 in
      (t3, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
  | Fun (x, Some ty, body) ->
      let env = Env.add x (Forall ([], ty)) env in
      let t_body, c_body = loop env body in
      (TFun (ty, t_body), c_body)
  | Fun (arg, None, body) ->
    let fresh_arg = TVar (gensym ()) in
    let env = Env.add arg (Forall ([], fresh_arg)) env in
    let t_body, c_body = loop env body in
    (TFun (fresh_arg, t_body), c_body)   
  | App (e1, e2) ->
    let t_fun, c_fun = loop env e1 in
    let t_arg, c_arg = loop env e2 in
    let fresh = TVar (gensym ()) in
    let constraints = (t_fun, TFun (t_arg, fresh)) :: c_fun @ c_arg in
    (fresh, constraints)
  | Let { is_rec = false; name; value; body } ->
    let t_val, c_val = loop env value in
    let env = Env.add name (Forall ([], t_val)) env in
    let t_body, c_body = loop env body in
    (t_body, c_val @ c_body)
  | Let { is_rec = true; name; value; body;} ->
    let fresh1 = TVar (gensym ()) in  
    let fresh2 = TVar (gensym ()) in  
    let env_with_f = Env.add name (Forall ([], TFun (fresh1, fresh2))) env in  
    let _, c_val = loop env_with_f value in
    let env_with_f_for_body = Env.add name (Forall ([], TFun (fresh1, fresh2))) env in
    let t_body, c_body = loop env_with_f_for_body body in
    let c = c_val @ c_body in
    (t_body, c)
  | Assert False -> (TVar (gensym ()), [])
  | Assert e ->
    let t, c = loop env e in
    (TUnit, (t, TBool) :: c)  
  | Annot (e, ty) ->
      let t, c = loop env e in
      (ty, (t, ty) :: c)
  | PairMatch { matched; fst_name; snd_name; case } ->
    let fresh1 = TVar (gensym ()) in
    let fresh2 = TVar (gensym ()) in
    let t_matched, c_matched = loop env matched in
    let extended_env = Env.add fst_name (Forall ([], fresh1)) (Env.add snd_name (Forall ([], fresh2)) env) in
    let t_case, c_case = loop extended_env case in
    (t_case, (t_matched, TPair (fresh1, fresh2)) :: c_matched @ c_case)
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
    let fresh = TVar (gensym ()) in
    let t_matched, c_matched = loop env matched in
    let env_hd = Env.add hd_name (Forall ([], fresh)) env in
    let env_tl = Env.add tl_name (Forall ([], TList fresh)) env_hd in
    let t_cons_case, c_cons_case = loop env_tl cons_case in
    let t_nil_case, c_nil_case = loop env nil_case in
    let constraints =
      (t_matched, TList fresh)
      :: (t_cons_case, t_nil_case)
      :: c_matched @ c_cons_case @ c_nil_case
    in
    (t_nil_case, constraints)
    
in
try
  let t, c = loop env exp in
  let t = unify t c in
  match t with
  | Some t -> Some t 
  | None -> None
with _ -> None

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let rec eval_expr env exp =
  match exp with
  | Unit -> VUnit
  | True -> (VBool true)
  | False -> (VBool false)
  | Nil -> VList []
  | ENone -> VNone
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x -> ( Env.find x env)
  | Assert e -> (
      match eval_expr env e with
      | VBool true -> VUnit
      | _ -> raise AssertFail
    )
  | ESome e -> VSome (eval_expr env e)
  | Bop (op, e1, e2) ->
    (
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
     eval_bop op v1 v2)
  | If (cond, e_then, e_else) -> (
      match eval_expr env cond with
      | VBool true -> eval_expr env e_then
      | VBool false -> eval_expr env e_else
      | _ -> failwith "Condition of if must be a boolean"
    )
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> (
      match eval_expr env matched with
      | VList [] -> eval_expr env nil_case
      | VList (hd :: tl) ->
          let env' = Env.add hd_name hd (Env.add tl_name (VList tl) env) in
          eval_expr env' cons_case
      | _ -> failwith "List match requires a list"
    )
  | OptMatch { matched; some_name; some_case; none_case } -> (
      match eval_expr env matched with
      | VSome v ->
          let env' = Env.add some_name v env in
          eval_expr env' some_case
      | VNone -> eval_expr env none_case
      | _ -> failwith "Option match requires an option"
    )
  | PairMatch { matched; fst_name; snd_name; case } -> (
      match eval_expr env matched with
      | VPair (v1, v2) ->
          let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
          eval_expr env' case
      | _ -> failwith "Pair match requires a pair"
    )
  | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
  | App (f, e) -> (
      match eval_expr env f with
      | VClos { name = _; arg; body; env = clos_env } ->
          let arg_val = eval_expr env e in
          let env' = Env.add arg arg_val clos_env in
          eval_expr env' body
      | _ -> failwith "Application requires a function"
    )
  | Annot (e, _) -> eval_expr env e
  | Let { is_rec; name; value; body } ->
    if is_rec then
      (match eval_expr env value with
      | VClos { name=n; arg = param; body = nbody; env = closure_env } -> 
        if (n = None) then
          let value_val = VClos {name=Some name; arg = param; body=nbody; env = closure_env} in
          eval_expr (Env.add name value_val env) body
        else raise RecWithoutArg
      | _ -> failwith "Not Valid!")
    else
      let value_val = eval_expr env value in
      eval_expr (Env.add name value_val env) body
  and eval_bop op v1 v2 =
    match (op,v1,v2)with
    | (Add, VInt n1, VInt n2) -> VInt (n1 + n2)
    | (Sub, VInt n1, VInt n2) -> VInt (n1 - n2)
    | (Mul, VInt n1, VInt n2) -> VInt (n1 * n2)
    | (Div, VInt _, VInt 0) -> raise DivByZero
    | (Div, VInt n1, VInt n2) -> VInt (n1 / n2)
    | (Mod, VInt _, VInt 0) -> raise DivByZero
    | (Mod, VInt n1, VInt n2) -> VInt (n1 mod n2)
    | (AddF, VFloat n1, VFloat n2) -> VFloat (n1 +. n2)
    | (SubF, VFloat n1, VFloat n2) -> VFloat (n1 -. n2)
    | (MulF, VFloat n1, VFloat n2) -> VFloat (n1 *. n2)
    | (DivF, VFloat n1, VFloat n2) -> VFloat (n1 /. n2)
    | (PowF, VFloat n1, VFloat n2) -> VFloat (n1 ** n2)
    | (Lt, VInt n1, VInt n2) -> VBool (n1 < n2)
    | (Lt, VFloat n1, VFloat n2) -> VBool (n1 < n2)
    | (Lt, _, _) -> raise CompareFunVals
    | (Lte, VInt n1, VInt n2) -> VBool (n1 <= n2)
    | (Lte, VFloat n1, VFloat n2) -> VBool (n1 <= n2)
    | (Lte, _, _) -> raise CompareFunVals
    | (Gt, VInt n1, VInt n2) -> VBool (n1 > n2)
    | (Gt, VFloat n1, VFloat n2) -> VBool (n1 > n2)
    | (Gt, _, _) -> raise CompareFunVals
    | (Gte, VInt n1, VInt n2) -> VBool (n1 >= n2)
    | (Gte, VFloat n1, VFloat n2) -> VBool (n1 >= n2)
    | (Gte, _, _) -> raise CompareFunVals
    | (Eq, v1, v2) -> VBool (v1 = v2)
    | (Neq, v1, v2) -> VBool (v1 <> v2)
    | (And, VBool false, _) -> VBool false
    | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
    | (Or, VBool true, _) -> VBool true
    | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
    | (Concat, VList v1, VList v2) -> VList (v1 @ v2)
    | (Cons, v1, VList v2) -> VList (v1 :: v2)
    | (Comma, v1, v2) -> VPair (v1, v2)
    | _ -> failwith "BOP error"

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
