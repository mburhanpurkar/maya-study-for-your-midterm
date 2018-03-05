module type SET =
  sig
    type elt
    type set
    val empty : set
    val is_member : elt -> set -> bool
    val add : elt -> set -> set
    val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a
    val delete : elt -> set -> set
  end

module type SETARG =
  sig
    type t
    val equal : t -> t -> bool
  end

module MakeSet (Set : SETARG) : (SET with type elt = Set.t) =
struct
  type elt = Set.t
  type set = elt list
  let empty = []
  let rec is_member x = List.exists (Set.equal x)
  let add x s = if is_member x s then s else x :: s
  let rec delete x = List.filter (fun a -> not (Set.equal a x))
  let fold = List.fold_left
end

module FloatArg =
  struct
    type t = float
    let equal x y = abs_float (x -. y) < 0.00001
  end

module FloatSet = MakeSet (FloatArg)
