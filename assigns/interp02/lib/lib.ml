open Utils

let parse = My_parser.parse

let desugar prog =
  let rec desugar_toplets = function
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: rest ->
        let function_type =
          List.fold_right(fun (_, arg_ty) acc -> FunTy(arg_ty, acc)) (args) ty
        in
        let desugared_value =
          List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc)) args (desugar_expr value)
        in
        Let { is_rec; name; ty = function_type; value = desugared_value; body = desugar_toplets rest }
  and desugar_expr = function
    | SLet { is_rec; name; args; ty; value; body } ->
        let function_type =
          List.fold_right (fun (_, arg_ty) acc -> FunTy(arg_ty, acc)) (args) ty
        in
        let desugared_value =
          List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc)) args (desugar_expr value)
        in
        Let { is_rec; name; ty = function_type; value = desugared_value; body = desugar_expr body;
        }
    | SFun { arg; args; body } ->
        List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc)) (arg :: args) (desugar_expr body)
    | SIf (cond, then_, else_) ->
        If (desugar_expr cond, desugar_expr then_, desugar_expr else_)
    | SApp (e1, e2) ->
        App (desugar_expr e1, desugar_expr e2)
    | SBop (op, e1, e2) ->
        Bop (op, desugar_expr e1, desugar_expr e2)
    | SAssert e ->
        Assert (desugar_expr e)
    | SUnit -> Unit
    | STrue -> True
    | SFalse -> False
    | SNum n -> Num n
    | SVar x -> Var x
  in
  desugar_toplets prog

(*
  let interp (input : string) : (value, error) result =
    match parse input with
    | Some prog -> (
        let expr = desugar prog in
        match type_of expr with
        | Ok _ -> Ok (eval expr)
        | Error e -> Error e
    )
    | None -> Error ParseErr
    *)