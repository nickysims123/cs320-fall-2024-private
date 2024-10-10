(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Last Function Standing
*)

let get_life func start pred =
  let rec loop value steps =
    if pred value then steps  
    else loop (func value) (steps + 1)  
  in
  loop start 0  

let rec find_func funcs num acc = 
  match funcs with 
  | [] -> None 
  | h::t -> if acc = num then Some h else find_func t num (acc + 1)

  let rec check_dupes2 lifes life = 
    match lifes with 
    | [] -> false
    | h::t -> if h = life then true else check_dupes2 t life  

let rec check_dupes1 lifes life = 
  match lifes with 
  | [] -> false
  | h::t -> if h = life then check_dupes2 t life else check_dupes1 t life

let check_dupes lifes max = 
  let rec loop lifespans = 
  match lifespans with 
  | [] -> false
  | h::t -> if h = max && check_dupes1 lifes h then true else loop t
  in loop lifes

let last_function_standing funcs start pred =
  if funcs = [] then None  
  else
    let lifespans = List.map (fun f -> get_life f start pred) funcs in
    let max_lifespan = List.fold_left max (-1) lifespans in
    if check_dupes lifespans max_lifespan then None else
    let rec loop lifes max acc = 
    match lifes with 
    | [] -> None
    | h::t -> if h = max_lifespan then find_func funcs acc 0 else loop t max (acc + 1)
    in loop lifespans max_lifespan 0

