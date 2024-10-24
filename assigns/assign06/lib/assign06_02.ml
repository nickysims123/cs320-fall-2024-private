(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Your First (RPN) Parser
*)

open Utils

let parse l = 
  let rec loop lst acc =
    match lst with 
    | [] -> (
            match acc with
            | [exp] -> Some exp
            | _ -> None
            )
    | TNum(value)::t -> loop t ((Num(value))::acc)
    | TAdd::t-> (
                match acc with
                | expr2::expr1::t2 -> loop t (Add(expr1, expr2)::t2)
                | _ -> None
                )
    | TLt::t-> (
                match acc with 
                | expr2::expr1::t2 -> loop t (Lt(expr1,expr2)::t2)
                | _ -> None
                )
    | TIte::t -> (
                  match acc with 
                  | expr3::expr2::expr1::t2 -> loop t (Ite(expr1,expr2,expr3)::t2)
                  | _ -> None
                )              
  in loop l []