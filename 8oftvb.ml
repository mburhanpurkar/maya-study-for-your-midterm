(* Tuples and dictionaries and things *)

(* Let's make some methods for dictionaries *)

exception Not_found
let rec lookup (x : 'a) (l : ('a * 'b) list) : 'b =
  match l with
  | [] -> raise Not_found
  | (a, b)::t -> if x = a then b else lookup a t

let rec add (k : 'a) (v : 'b) (d : ('a * 'b) list) : ('a * 'b) list =
  match d with
  | [] -> [(k, v)]
  | (k', v')::t ->
    if k = k' then (k, v) :: t
    else (k', v') :: add k v t

let rec remove (k : 'a) (d : ('a * 'b) list) : ('a * 'b) list =
  match d with
  | [] -> raise Not_found
  | (k', v')::t ->
    if k = k' then t
    else (k', v') :: remove k t

let rec key_exists (k : 'a) (d : ('a * 'b) list) : bool =
  try
    let _ = remove k d in true
  with
    Not_found -> false

(* Q1 *)
let rec size : ('a * 'b) list -> int = List.length

(* Q2 *)
let rec replace (k : 'a) (v : 'b) (d : ('a * 'b) list) : ('a * 'b) list =
  match d with
  | [] -> raise Not_found
  | (k', v')::t ->
    if k = k' then (k, v) :: t
    else (k', v') :: replace k v t

(* Q3 *)
let generate (k : 'a list) (v : 'b list) : ('a * 'b) list =
  let rec generate_helper (k : 'a list) (v : 'b list) (seen : 'a list) : ('a * 'b) list =
    match k, v with
    | [], [] -> []
    | [], l | l, [] -> raise (Invalid_argument "k and v of unequal length")
    | (k'::tk),(v'::tv) -> if List.mem k' seen then generate_helper tk tv seen
      else (k', v') :: generate_helper tk tv (k' :: seen)
  in
  generate_helper k v []

(* Q4 - REVIEW THIS MAYA YOU'RE SO DUMB SOMETIMES *)
let rec ungenerate (d : ('a * 'b) list) : ('a list * 'b list) =
  match d with
  | [] -> ([], [])
  | (k, v)::t ->
    match ungenerate t with
    | (k2, v2) -> (k::k2, v::v2)

(* Q5 *)
let rec make_dict : ('a * 'b) list -> ('a * 'b) list =
  List.fold_left (fun a (k, v) -> add k v a) []

(* Q6 *)
let rec union (d1 : ('a * 'b) list) (d2 : ('a * 'b) list) : ('a * 'b) list =
  match d1 with
  | [] -> d2
  | (k, v)::t -> union t (add k v d2)
