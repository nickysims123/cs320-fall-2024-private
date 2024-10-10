(*
Nicholas Sima
U39334521 
CS320 Fall 2024 A1

Walking Distance

dist: computes the distance given
a list of directions
*)

type dir = 
| North
| South
| East
| West

type path = dir list

let dist walks : float = 
  let x = 0 in
  let y = 0 in
  let rec walk walks x y =
    match walks with
    | [] -> sqrt (float_of_int ((x * x) + (y * y)))
    | h::t -> match h with
              | North -> walk t x (y+1)
              | South -> walk t x (y-1)
              | East -> walk t (x+1) y
              | West -> walk t (x-1) y
  in walk walks x y