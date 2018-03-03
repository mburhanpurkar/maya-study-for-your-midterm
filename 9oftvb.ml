(* Partial Application and currying (nom nom nom) *)

(* Q2 *)
let member_all (x : 'a) (ll : 'a list list) =
  let member (x : 'a) : 'a list -> bool =
    List.fold_left (fun a h -> h = x || a) false
  in
  List.fold_left (fun a h -> h && a) true (List.map (member x) ll)

(* Q3 *)
let halve : int list -> int list = List.map (fun x -> x / 2)

(* Q4 *)
let mapll (f : 'a -> 'b) : 'a list list list -> 'b list list list =
  List.map (List.map (List.map f))

(* Q5 *)
let truncate (n : int) : 'a list list -> 'a list list =
  let rec take (n : int) (l : 'a list) =
    if n = 0 then [] else
      match l with h::t -> h :: take (n - 1) t
  in
  let rec truncate_helper (n : int) (l : 'a list) : 'a list =
    if List.length l >= n then take n l else l
  in
  List.map (truncate_helper n)

(* Q6 *)
let rec firsts : int list list -> int list =
  let get_head (l : int list) : int = match l with h::_ -> h | [] -> 0 in
  List.map get_head
