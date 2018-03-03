(* Mapping

   Anonymous functions are things for example fun x -> x + 2
   Also, mapping exists

*)

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

(* Q1 *)
let calm : char list -> char list = map (fun x -> if x = '!' then '.' else x)

(* Q2 / Q3*)
let clip : int list -> int list =
  map (fun x -> if x > 10 then 10 else if x < 0 then 0 else x)

(* Q4 *)
let rec apply (f : 'a -> 'b) (x : 'a) (n : int) : 'b =
  if n = 0 then x else apply f (f x) (n - 1)

(* Q6 *)
let filter (f : 'a -> bool) : 'a list -> 'a list =
  List.fold_left (fun a h -> if f h then h :: a else a) []

(* Q7 *)
let for_all (f : 'a -> bool) : 'a list -> bool =
  List.fold_left (fun a h -> f h && a) true

(* Q8 *)
let mapl (f : 'a -> 'b) : 'a list list -> 'b list list = map (map f)
