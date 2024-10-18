(*
Nicholas Sima
U39334521
CS 320 Fall 2024 A1

Your First Lexer
*)

open Utils

let lex s = 
  let rec loop lst acc = 
    match lst with 
    | [] -> Some acc
    | h::t -> (
              match h with 
              | "+" -> loop t (acc@[TAdd])
              | "<" -> loop t (acc@[TLt])
              | "?" -> loop t (acc@[TIte])
              | _ -> match (int_of_string_opt h) with
                      | None -> None
                      | _ -> loop t (acc@[TNum(int_of_string h)])
              )
  in loop (split s) []