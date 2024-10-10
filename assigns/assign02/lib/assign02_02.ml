(*
Nicholas Sima
U39334521 
CS320 Fall 2024 A1

Matrices


*)

type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}


let mk_matrix l1 r_c = 
let row,col = r_c in 
let rec loop main_lst full_lst curr_lst col acc =
  match main_lst with
  | [] -> (full_lst@[curr_lst])
  | h::t -> if acc < col then loop t full_lst (curr_lst@[h]) col (acc+1)
            else loop main_lst (full_lst@[curr_lst]) [] col 0
in let entries1 = if l1 = [] then [] else loop l1 [] [] col 0
in {entries = entries1; rows = row; cols = col}