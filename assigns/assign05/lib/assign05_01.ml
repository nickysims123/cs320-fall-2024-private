(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Folding and OUnit2
*)

type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let rec fold_left op base test = 
  match test with 
  | TestCase test1 -> op base test1
  | TestList tests -> List.fold_left (fold_left op) base tests

  