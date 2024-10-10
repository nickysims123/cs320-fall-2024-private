(*
Nicholas Sima U39334521
CS320 Fall 2024 A1
Integer Powers

assign01_01.ml - pow()

inputs: 
integer n
integer k

returns:
n^k (n to the power k)
*)

let pow (n : int) (k : int) : int =
  if k = 0 then 1 else
    let rec loop (n : int) (k : int) (acc : int) : int = 
      if k = 1 then acc
      else loop n (k - 1) (acc * n)
    in
    loop n k n 