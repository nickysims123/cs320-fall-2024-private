(*
Second Ever OCAML Program

defining function is_prime - inputs
an integer and determines its primality
*)

let is_prime n = 
  if n < 2 then false else
  let rec loop n i = 
    if i > (n / 2) then true
    else if n mod i = 0 then false
    else loop n (i + 1)
  in
  loop n 2