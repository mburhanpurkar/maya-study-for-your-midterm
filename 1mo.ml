(* General Folding *)

let map (f : 'a -> 'b) (l : 'a list) : 'b list =
  List.fold_right (fun h a -> f h :: a) l []

let copy (l : 'a list) : 'a list = List.fold_right (fun h a -> h :: a) l []

let append (x : 'a) (l : 'a list) : 'a list =
  List.fold_right (fun h a -> h :: a) l [x]

let split (l : ('a * 'b) list) : 'a list * 'b list =
  List.fold_right (fun (hx, hy) (ax, ay) -> hx :: ax, hy :: ay) l ([], [])


(* Folding Over Trees *)

type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree

(* If I understand correctly, it seems that e is the initial value of the
   accumulator and f takes in three arguments: the root and the two accs for
   each of the branches. *)
let rec fold_tree (f : 'a -> 'b -> 'b -> 'b) (e : 'b) (t : 'a tree) : 'b =
  match t with
  | Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let tree_size : 'a tree -> int =
  fold_tree (fun _ a1 a2 -> 1 + a1 + a2) 0

let tree_sum : 'a tree -> int =
  fold_tree (fun r a1 a2 -> r + a1 + a2) 0

(* left-to-right bottom-to-top tree traversal, I think *)
let tree_traversal : 'a tree -> 'a list =
  fold_tree (fun r a1 a2 -> [r] @ a1 @ a2) []


(* Q1 - loving the unreadability :) *)
let expenses : int -> int list -> int =
  List.fold_left ( - )

(* Q2 *)
let len : 'a list -> int =
  List.fold_left (fun a _ -> 1 + a) 0

(* Q3 *)
let get_last : 'a list -> 'a =
  List.fold_left (fun _ h -> h) 0

(* Q4 *)
let rev : 'a list -> 'a list =
  List.fold_left (fun a h -> h :: a) []

(* Q5 *)
let mem (x : 'a) : 'a list -> bool =
  List.fold_left (fun a h -> h = x || a) false

(* Q6 *)
let str_combine : string list -> string =
  List.fold_left (fun a h -> if a = "" then a ^ h else a ^ " " ^ h) ""

(* Q7 *)
let max_tree_depth : 'a tree -> int =
  fold_tree (fun _ a1 a2 -> 1 + max a1 a2) 0
