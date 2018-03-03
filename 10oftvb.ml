(* Variant Types *)

(* Define a rectangle type with some methods *)
type rect =
  | Rectangle of int * int
  | Square of int

let area (x : rect) : int =
  match x with
  | Rectangle (l, w) -> l * w
  | Square a -> a * a

let rotate (x : rect) : rect =
  match x with
  | Rectangle (l, w) -> if w > l then x else Rectangle (w, l)
  | Square a -> x

let sort (l : rect list) : rect list =
  let extract_length (r : rect) : int =
    match r with
    | Square a -> a
    | Rectangle (l, _) -> l
  in
  let cmp (r1 : rect) (r2 : rect) : int =
    let l = extract_length r1 in
    let l' = extract_length r2 in
    if l > l' then 1 else if l = l' then 0 else -1
  in
  List.sort cmp (List.map rotate l)


(* Take, drop, map for a sequence type *)
type 'a sequence =
  | Nil
  | Cons of 'a * 'a sequence

let rec take (n : int) (l : 'a sequence) : 'a sequence =
  if n = 0 then Nil else
  match l with
  | Cons (a, b) -> Cons(a, take (n - 1) b)
  | Nil -> raise (Invalid_argument "n")

let rec drop (n : int) (l : 'a sequence) : 'a sequence =
  if n = 0 then l else
  match l with
  | Cons (_, b) -> take (n - 1) b
  | Nil -> raise (Invalid_argument "n")

let rec map (f : 'a -> 'b) (l : 'a sequence) : 'b sequence =
  match l with
  | Cons (a, b) -> Cons (f a, map f b)
  | Nil -> Nil
