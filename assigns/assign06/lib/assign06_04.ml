(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

An Evaluator
*)

open Utils

let rec eval exp = 
  match exp with 
  | Num(value) -> VNum value
  | Add(exp1, exp2) -> (
                      match (eval exp1), (eval exp2) with 
                      | VNum val1, VNum val2 -> VNum (val1 + val2)
                      | _ -> raise Not_found
                      )
  | Lt(exp1, exp2) -> (
                      match (eval exp1), (eval exp2) with
                      | VNum val1, VNum val2 -> VBool (val1 < val2)
                      | _ -> raise Not_found
                      )
  | Ite(exp1, exp2, exp3) -> (
                              match (eval exp1) with
                              | VBool check -> if check then eval exp2 else eval exp3
                              | _ -> raise Not_found
                              )