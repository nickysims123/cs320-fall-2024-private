(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Your first evaluator
*)

open Assign04_02

type value = 
| VNum of int
| VBool of bool

let get_vals expr1 expr2 = 
  let v1 = match expr1 with
  | VNum(e1) -> e1
  | _ -> 0
  in match expr2 with 
  | VNum(e2) -> (v1, e2)
  | _ -> (0,0)

let add (v1, v2) = v1 + v2

let rec eval expr = 
  match expr with 
  | True -> (VBool true)
  | False -> (VBool false)
  | Num(v) -> (VNum v)
  | Or(expr1, expr2) -> if eval expr1 = (VBool true) || eval expr2 = (VBool true) then (VBool true) else (VBool false)
  | Add(expr1, expr2) -> let v1 = eval expr1 in let v2 = eval expr2 in VNum(add(get_vals v1 v2))
  | IfThenElse(check, expr1, expr2) -> if eval check = (VBool true) then eval expr1 else eval expr2
