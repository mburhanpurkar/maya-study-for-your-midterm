(* First, define a signature for all the diffferent set implementations we will build *)
module type SetType =
  sig
    type elt
    type set
    val empty : set
    val set_of_list : elt list -> set
    val list_of_set : set -> elt list
    val insert : elt -> set -> set
    val size : set -> int
    val member : elt -> set -> bool
  end

(* Define a list implementation *)
module SetList : sig include SetType end =
  struct
    type elt
    type set = elt list
    let empty = []
    let member = List.mem
    let insert x l = if member x l then l else x :: l
    let rec set_of_list l =
      match l with
      | [] -> []
      | h::t -> insert h (set_of_list t)
    let list_of_set x = x
    let size = List.length
  end

(* For practice, let's make a functor for the set list implementation *)
module type Comparable =
  sig
    type t
    val compare : t -> t -> int
  end

module MakeListSet (Val : Comparable) : (SetType with type elt = Val.t) =
  struct
    type elt = Val.t
    type set = elt list
    let empty = []
    let member = List.mem
    let insert x l = if member x l then l else x :: l
    let rec set_of_list l =
      match l with
      | [] -> []
      | h::t -> insert h (set_of_list t)
    let list_of_set x = x
    let size = List.length
  end

(* Now, let's try making an int set *)
module IntCmp : (Comparable with type t = int) =
  struct
    type t = int
    let compare x y = if x > y then 1 else if x = y then 0 else -1
  end

module IntSet = MakeListSet (IntCmp)

let set1 =
  let open IntSet in
  insert 2 (insert 1 (insert 0 empty))

(* val set1 : IntSet.set = <abstr> *)

(* IntSet.member 2 set1;;
- : bool = true

IntSet.member 3 set1;;
- : bool = false *)

(* Define a binary tree implementation *)
(* module SetTree : sig include SetType end =
  struct
    type 'a t = Lf | Br of 'a t * 'a * 'a t

    let rec member x t =
      match t with
      | Lf -> false
      | Br (t1, a, t2) -> a = x || if x > a then member x t2 else member x t1

    let rec insert x t =
      match t with
      | Lf -> Br (Lf, x, Lf)
      | Br (t1, a, t2) ->
        if x = a then Br (t1, a, t2)
        else if x > a then Br (t1, a, insert x t2)
        else Br (insert x t1, a, t2)

    let rec set_of_list l =
      match l with
      | [] -> Lf
      | h::t -> insert h (set_of_list t)

    let rec list_of_set t =
      match t with
      | Lf -> []
      | Br (t1, a, t2) -> list_of_set t1 @ [a] @ list_of_set t2

    let rec size t =
      match t with
      | Lf -> 0
      | Br (t1, _, t2) -> 1 + size t1 + size t2

  end *)
