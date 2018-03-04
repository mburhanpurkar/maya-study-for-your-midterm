(* Treeeeees *)

type 'a tree  =
  | Br of 'a * 'a tree * 'a tree
  | Lf

let rec size (t : 'a tree) : int =
  match t with
  | Br (_, t1, t2) -> 1 + size t1 + size t2
  | Lf -> 0

let rec total (t : 'a tree) : int =
  match t with
  | Br (a, t1, t2) -> a + total t1 + total t2
  | Lf -> 0

let rec max_depth (t : 'a tree) : int =
  match t with
  | Br (_, t1, t2) -> 1 + max (max_depth t1) (max_depth t2)
  | Lf -> 0

let rec list_of_tree (t : 'a tree) : 'a list =
  match t with
  | Br (a, t1, t2) -> (list_of_tree t1) @ [a] @ (list_of_tree t2)
  | Lf -> []

let rec tree_map (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Br (a, t1, t2) -> Br (f a, tree_map f t1, tree_map f t2)
  | Lf -> Lf

let rec lookup (k : 'a) (t : ('a * 'b) tree) : 'b =
  match t with
  | Br ((k', v'), t1, t2) ->
      if k' = k then v'
      else if k' > k then lookup k t1
      else lookup k t2
  | Lf -> raise Not_found

let rec lookup_opt (k : 'a) (t : ('a * 'b) tree) : 'b option =
  match t with
  | Br ((k', v'), t1, t2) ->
      if k' = k then Some v'
      else if k' > k then lookup_opt k t1
      else lookup_opt k t2
  | Lf -> None

let rec insert (k : 'a) (v : 'b) (t : ('a * 'b) tree) : ('a * 'b) tree =
  match t with
  | Br ((k', v'), t1, t2) ->
      if k' = k then Br ((k', v), t1, t2)
      else if k' > k then Br ((k', v'), insert k v t1, t2)
      else Br ((k', v'), t1, insert k v t2)
  | Lf -> Br ((k, v), Lf, Lf)


(* Q1 *)
let rec is_element (x : 'a) (t : 'a tree) : bool =
  match t with
  | Br (a, t1, t2) -> a = x || if a < x then is_element x t2 else is_element x t1
  | Lf -> false

(* Q2 *)
let rec flip_tree (t : 'a tree) : 'a tree =
  match t with
  | Br (a, t1, t2) -> Br (a, flip_tree t2, flip_tree t1)
  | Lf -> Lf

(* Q3 *)
let rec same_shape (t1 : 'a tree) (t2 : 'b tree) : bool =
  match t1, t2 with
  | Br (_, t1, t2), Br (_, t1', t2') -> same_shape t1 t1' && same_shape t2 t2'
  | Lf, Lf -> true
  | _ -> false

(* Q4 *)
let rec tree_of_list (l : ('a * 'b) list) : ('a * 'b) tree =
  match l with
  | (k, v)::t -> insert k v (tree_of_list t)
  | [] -> Lf

(* Q5 *)
let rec combine (d1 : ('a * 'b) tree) (d2 : ('a * 'b) tree) : ('a * 'b) tree =
  match d1 with
  | Br ((k, v), t1, t2) -> combine t2 (combine t1 (insert k v d2))
  | Lf -> d2
