(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Grouping
*)

let group lst : int list list option = 
  let rec loop l = 
    match l with
    | [] -> None
    | h::t -> if h <> 3 then None else loop t
  in loop lst