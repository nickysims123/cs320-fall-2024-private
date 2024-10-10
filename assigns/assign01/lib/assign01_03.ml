(*
Nicholas Sima U39334521
CS320 Fall 2024 A1
Integers Encoding Sequences

assign01_03.ml - nth

*)

open Assign01_01

let is_prime n = 
  if n < 2 then false else
  let rec loop n i = 
    if i > (n / 2) then true
    else if n mod i = 0 then false
    else loop n (i + 1)
  in
  loop n 2

let nth (s : int) (i : int) : int = 
  let rec loop_prime (s : int) (i : int) (acc : int) (prime : int) : int =
    if is_prime acc && prime = i then 
      let rec loop_exp (s : int) (num : int) (exp : int) : int = 
        if s mod (pow num exp) = 0 then loop_exp s num (exp +1)
        else
          exp - 1
      in loop_exp s acc 1
    else
      if is_prime acc then 
        loop_prime s i (acc + 1) (prime + 1)
      else
        loop_prime s i (acc + 1) prime
  in loop_prime s i 2 0