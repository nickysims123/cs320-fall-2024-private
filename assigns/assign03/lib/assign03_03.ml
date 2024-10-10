(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Tree Collapsing
*)

type tree = | Leaf of int | Node of tree list

let collapse k main_tree = 
  if main_tree = Leaf (k) then main_tree else main_tree