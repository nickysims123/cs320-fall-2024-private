(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Your second type checker
*)

type ident = string 

type expr' = 
| True
| False
| Num of int
| Var of ident
| Let of ident * expr' * expr'
| Add of expr' * expr'
| Or of expr' * expr'
| IfThenElse of expr' * expr' * expr'

type ty' = 
| Int 
| Bool 

type context = (ident * ty') list

let rec check_context cont variable = 
  match cont with 
  | [] -> None
  | (string, value)::t -> if string = variable then Some value else check_context t variable

let convert_option opt_value = 
  match opt_value with 
  | None -> raise Not_found
  | Some value -> value

let add_context cont vars = 
  vars :: cont

let rec type_of' cont expr = 
  match expr with 
  | True -> Some Bool
  | False -> Some Bool
  | Num(e) -> if e = 0 then Some Int else Some Int 
  | Var(var) -> check_context cont var 
  | Let(var, expr1, expr2) -> if type_of' cont expr1 = None then 
                              None else type_of' (add_context cont (var, convert_option(type_of' cont expr1))) expr2
  | Add(expr1, expr2) -> if type_of' cont expr1 = Some Int && type_of' cont expr2 = Some Int then Some Int else None 
  | Or(expr1, expr2) -> if type_of' cont expr1 = Some Bool && type_of' cont expr2 = Some Bool then Some Bool else None
  | IfThenElse(check, expr1, expr2) -> if type_of' cont check = Some Bool then if type_of' cont expr1 = type_of' cont expr2 then type_of' cont expr1 else None else None 
                                          