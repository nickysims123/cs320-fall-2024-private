(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

A Type Checker
*)

open Utils

let rec type_of exp =
  match exp with
  | Num(_) -> Some TInt
  | Add(exp1, exp2) -> (
                        match type_of exp1, type_of exp2 with
                        | Some TInt, Some TInt -> Some TInt 
                        | _ -> None
                        )
  | Lt(exp1, exp2) -> (
                      match type_of exp1, type_of exp2 with 
                      | Some TInt, Some TInt -> Some TBool
                      | _ -> None
                      )          
  | Ite(exp1, exp2, exp3) -> (
                              match type_of exp1, type_of exp2, type_of exp3 with
                              | Some TBool, ty1, ty2 -> if ty1 = ty2 then ty1 else None
                              | _ -> None
                              )           