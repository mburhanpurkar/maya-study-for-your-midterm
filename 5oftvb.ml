(* Sorting *)


(* Preliminaries *)
let rec take (n : int) (l : 'a list) =
  if n = 0 then [] else
    match l with h::t -> h :: take (n - 1) t

let rec drop (n : int) (l : 'a list) =
  if n = 0 then l else
    match l with _::t -> take (n - 1) t


(* Insertion sort *)
let rec isort (l : 'a list) : 'a list =
  (* Inserts x into a sorted list l *)
  let rec insert (x : 'a) (l : 'a list) =
    match l with
    | [] -> [x]
    | h::t -> if x <= h then x :: l else insert x t
  in
  match l with
  | [] -> []
  | h::t -> insert h (isort t)


(* Merge sort *)
let rec msort (l : 'a list) : 'a list =
  (* Merges two sorted lists *)
  let rec merge (x : 'a list) (y : 'a list) : 'a list =
    match x, y with
    | [], l | l, [] -> l
    | hx::tx, hy::ty ->
        if hx < hy then hx :: merge tx y
        else hy :: merge x ty
  in
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let left = take (List.length l / 2) l in
    let right = drop (List.length l / 2) l in
    merge (msort left) (msort right)
