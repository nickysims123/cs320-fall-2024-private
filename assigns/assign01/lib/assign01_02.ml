(*
Nicholas Sima U39334521
CS320 Fall 2024 A1
Primes Again

assign01_02.ml - nth_prime()

inputs: integer representing
the desired # of prime number

outputs: returns the nth
prime number
*)

let is_prime n = 
  if n < 2 then false else
  let rec loop n i = 
    if i > (n / 2) then true
    else if n mod i = 0 then false
    else loop n (i + 1)
  in
  loop n 2


let nth_prime (n : int) : int = 
  let rec loop (n : int) (i : int) (acc : int) =
    if is_prime i then 
      if n = acc then i
      else loop n (i + 1) (acc + 1)
    else
      loop n (i + 1) acc
  in
  loop n 2 0
