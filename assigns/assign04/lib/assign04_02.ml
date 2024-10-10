(*
Nicholas Sima
U39334521
CS320 Fall A1

Your first type checker
*)

type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

type ty = 
| Int 
| Bool 

let rec type_of expression = 
  match expression with
  | True -> Some Bool
  | False -> Some Bool
  | Num expr1 -> if expr1 = 0 then Some Int else Some Int
  | Or(expr1, expr2) -> if type_of expr1 = Some Bool && type_of expr2 = Some Bool then Some Bool else None
  | Add(expr1, expr2) -> if type_of expr1 = Some Int && type_of expr2 = Some Int then Some Int else None
  | IfThenElse(check, expr1, expr2) -> if type_of check = Some Bool && type_of expr1 = type_of expr2 then type_of expr1 else None


