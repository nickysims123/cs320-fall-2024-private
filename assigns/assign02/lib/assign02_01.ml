(*
Nicholas Sima
U39334521 
CS320 Fall 2024 A1

Tic-Tac-Toe


*)

type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom

type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index


let get_pos (board : board) (index : pos_index) : pos =
  let (row, col) = index in
  let (x1y3, x2y3, x3y3),
      (x1y2, x2y2, x3y2),
      (x1y1, x2y1, x3y1) = board in
  if row = Top then
    if col = Left then x1y3
    else if col = Middle then x2y3
    else x3y3
  else if row = Middle then
    if col = Left then x1y2
    else if col = Middle then x2y2
    else x3y2
  else 
    if col = Left then x1y1
    else if col = Middle then x2y1
    else x3y1


let check_row row : bool = 
  let pos1, pos2, pos3 = row in
  if pos1 = pos2 && pos2 = pos3 then true else false
let check_col col : bool =
  let pos1, pos2, pos3 = col in
  if pos1 = pos2 && pos2 = pos3 then true else false

let check_diag rows: bool = 
  let pos1, pos2, pos3 = rows in 
  if pos1 = pos2 && pos2 = pos3 then true else false

let winner (b : board) : bool = 
  let (x1y3, x2y3, x3y3),
      (x1y2, x2y2, x3y2),
      (x1y1, x2y1, x3y1) = b in
  if check_row (x1y3, x2y3, x3y3) then true else
  if check_row (x1y2, x2y2, x3y2) then true else
  if check_row (x1y1, x2y1, x3y1) then true else
  if check_col (x1y3, x1y2, x1y1) then true else
  if check_col (x2y3, x2y2, x2y1) then true else
  if check_col (x3y3, x3y2, x3y1) then true else 
  if check_diag (x1y3, x2y2, x3y1) then true else
  if check_diag (x3y3, x2y2, x1y1) then true else
  false


