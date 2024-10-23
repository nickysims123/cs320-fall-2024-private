(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Your First (RPN) Parser
*)

open Utils

let take_two l = 
  match l with
  | h1::h2::t -> Some (h1,h2)
  | _ -> None

let remove_two l =
  match l with 
  | h1::h2::t -> Some t
  | _ -> None 

let parse l = 
  let rec loop lst acc =
    match l with 
    | [] -> [] 
    | h::t -> (
              match h with 
              | TNum(value) -> loop t (h::acc)
              | TAdd -> 
              )
  in loop l []