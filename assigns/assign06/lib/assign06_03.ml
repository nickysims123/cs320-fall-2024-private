(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

A Type Checker
*)

open Utils

let type_of exp =
  match exp with
  | _ -> Some TInt