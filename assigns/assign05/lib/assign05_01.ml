(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Folding and OUnit2
*)

type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let fold_left op base test = 
  let rec loop op base test acc = 
    match test with
    [] -> acc
    | TestCase(test)::t -> loop op base t (acc @ [op base test]) 
    | TestList(tests)::t -> loop op base (tests@t) acc
  in loop op base test []