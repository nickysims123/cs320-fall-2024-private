(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Generalized Fibonacci (Tail Recursion)
*)

let gen_fib l k = 
  let rec loop l k = 
    match l with
    | [] -> 3
    | h::t -> if h = k then 4 else loop t k
  in loop l k