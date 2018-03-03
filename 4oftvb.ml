(* Lists

   Cons (::) adds a single element to the front of a list
   STOP USING COMMAS YOU SILLY THING

   Append (@) is bad since O(n)
*)

let is_nil : 'a list -> bool = ( = ) []

(*
let rec len_bad (l : 'a list) : int =
  match l with
  | [] -> 0
  | _::t -> 1 + len_bad t

let len_okay (l : 'a list) : int =
  let rec len_inner (l : 'a list) (n : int) : int =
    match l with
    | [] -> 0
    | _::t -> len_inner t (n + 1)
  in
  len_inner l 0
*)

let len : 'a list -> int = List.fold_left (fun a h -> a + 1) 0

(*
let rec sum_bad (l : int list) : int =
  match l with
  | [] -> 0
  | h::t -> h + sum_bad t
*)

let sum : int list -> int = List.fold_left (fun a h -> a + h) 0

let rec odds (l : 'a list) =
  match l with
  | h::_::t -> h :: odds t
  | _ -> l

let rec append (x : 'a) (l : 'a list) =
  match l with
  | h::t -> h :: append x t
  | [] -> [x]

let rec take (n : int) (l : 'a list) =
  if n = 0 then [] else
    match l with h::t -> h :: take (n - 1) t

let rec drop (n : int) (l : 'a list) =
  if n = 0 then l else
    match l with _::t -> take (n - 1) t

(* Q1 *)
let rec evens (l : 'a list) =
  match l with
  | _::m::t -> m :: evens t
  | _ -> []

(* Q2
let count_true_meh : bool list -> int =
  let rec count_inner (n : int) (l : bool list)  : int =
    match l with
    | h::t -> if h then count_inner (n + 1) t else count_inner n t
    | [] -> n
  in
  count_inner 0 *)

let count_true : bool list -> int =
  List.fold_left (fun a h -> if h then 1 + a else a) 0

(* Q4 *)
let rec drop_last (l : 'a list) =
  match l with
  | [] -> []
  | [x] -> []
  | _::t -> drop_last t

(* Q5
let rec mem_bad (x : 'a) (l : 'a list) =
  match l with
  | h::t -> h = x || mem_bad x t
  | [] -> false *)

let mem (x : 'a) : 'a list -> bool =
  List.fold_left (fun a h -> h = x || a) false

(* Q6 *)
let rec setify (l : 'a list) : 'a list =
  List.fold_left (fun a h -> if mem h l then a else h :: a) [] l

(* Q7 *)
let rev : 'a list -> 'a list =
  List.fold_left (fun a h -> h :: a) []

(* Q3 -- back to this now that rev is better *)
let palindromeify (l : 'a list) : 'a list = l @ rev l

let is_palindrome (l : 'a list) : bool = l = rev l
