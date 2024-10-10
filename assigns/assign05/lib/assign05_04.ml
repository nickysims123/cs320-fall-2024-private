(*
Nicholas Sima
U39334521
CS320 Fall 2024 A1

Sets
*)

type set_info = {
ind : int -> bool;
mn : int;
mx : int;
}

module ListSet = struct
  type t = int list
  let mem want t = List.mem want t

  let empty = []

  let singleton value = [value]

  let rec card t = 
    match t with
    | [] -> 0
    | _::tail -> 1 + card tail

  let union l1 l2 = 
  List.fold_right (fun x acc -> if mem x acc then acc else x :: acc) l1 l2

  (*
  let union l1 l2 = 
    let loop l1 l2 acc = 
      match l1, l2 with
      | h1::t1, h2::t2 -> if h1 <> h2 then (min h1 h2)::
    in loop l1 l2 []
  *)
end

module FuncSet = struct
  type t = set_info

  let mem want t = t.ind want

  let empty = {ind = (fun _ -> false); mn = 1; mx = 0}

  let singleton value = {ind = (fun value1 -> value1 = value ); mn = value; mx = value}

  let card t = 
    let rec loop acc count = 
      if acc > t.mx then count
      else if t.ind count then loop (acc+1) (count+1)
      else loop (acc+1) (count)
    in loop t.mn 0

  let union l1 l2 = 
    let ind1 value = l1.ind value || l2.ind value in 
    {ind = ind1; mn = min l1.mn l2.mn; mx = max l1.mx l2.mx}
  end