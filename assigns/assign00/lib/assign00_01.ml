(*
First Ever OCAML Program

defining function sqrt - inputs an integer, 
find the nearest square root of the integer
*)

let rec sqrt n = 
  let i = 1 in
  iterate n i 
and
iterate n i = 
  if (i * i) >= n then i else iterate n (i + 1)