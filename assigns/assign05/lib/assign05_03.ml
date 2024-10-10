(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Anonymous Functions
*)

type ident = string

type ty = 
| Unit
| Arr of ty * ty

type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

type ctxt = (ident * ty) list

let rec check_gamma cont variable = 
  match cont with
  | [] -> None
  | (var, var_type)::t -> if var = variable then Some var_type else check_gamma t variable

let add_gamma cont vars = 
  vars::cont

let rec type_of gamma e = 
  match e with 
  | Var(id) -> check_gamma gamma id
  | Fun(id, type1, expr) -> (match type_of (add_gamma gamma (id, type1)) expr with
                            | Some expr2 -> Some (Arr (type1, expr2))
                            | None -> None 
                            )
  | App(expr1, expr2) -> (let type1 = type_of gamma expr1 in 
                          let type2 = type_of gamma expr2 in 
                          match type1, type2 with
                          | Some (Arr(expr1_type, expr2_type)), Some new_type -> if expr1_type = new_type then Some expr2_type else None
                          | _ -> None
                          )