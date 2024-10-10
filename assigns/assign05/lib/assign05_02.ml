(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Continuation Style Passing
*)

type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree
  
let sum_tr tree = 
  match tree with
  | Leaf -> 0
  | _ -> 0