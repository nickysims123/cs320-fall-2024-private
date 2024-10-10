(*
Nicholas Sima U39334521
CS320 Fall 2024 A1
Sequences to Strings 

assign01_04.ml - to_string()

inputs:
a sequence of integers symbolized by a 
product of prime exponents

outputs: the array of integers
the sequence represents as a 
string with [] around each 
value and separated by semi colons
*)

open Assign01_01
open Assign01_02
open Assign01_03

let to_string (n : int) : string = 
  let rec loop_extract (n : int) (result : string) (acc : int) : string =
    if n = 1 then
      if acc = 0 then
        "[]"
      else
        result ^ "]"
    else
      let num = nth n acc 
      in
      let prime = nth_prime acc
      in
      if acc = 0 then 
        loop_extract (n / (pow prime num)) (result ^ "[" ^ string_of_int num) (acc + 1)
      else
        loop_extract (n / (pow prime num)) (result ^ "; " ^ string_of_int num) (acc + 1) 
  in
  loop_extract n "" 0