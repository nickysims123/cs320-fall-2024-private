(*  
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Unique Keys
*)

let rec find_key l key = 
  match l with
  | [] -> false
  | h::t -> let k = fst h in if k = key then true else find_key t key

let rec get_val l key value = 
  match l with
  | [] -> value
  | h::t -> let k = fst h in if k = key then let added_val = snd h in get_val t key (value + added_val) else get_val t key value

let update_key l key value =
  let new_list = [] in
  let rec loop l key value new_list = 
  match l with
  | [] -> [(key, value)]@new_list
  | h::t -> let k = fst h in let v = snd h in if k = key then loop t key (get_val new_list key (value + v)) new_list else loop t key value ([(k,v)]@new_list)
  in loop l key value new_list

let mk_unique_keys l = 
  let new_list = [] in
  let rec loop l new_list = 
    match l with
    | [] -> new_list
    | h::t -> let k = fst h in if find_key new_list k then let v = snd h in loop t (update_key new_list k v) else loop t ([h]@new_list)
    in loop l new_list    