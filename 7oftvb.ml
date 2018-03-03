(* Exceptions

   Exceptions can be defined in two ways:
   exception Problem;;
   exception Problem of int;;

   Some examples below:
*)

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0

exception Empty of string
let rec last l =
  match l with
  | [] -> raise (Empty "l")
  | [x] -> x
  | _::t -> last t

(* Q1 - smallest positive element in list *)
exception Not_found
let smallest (l : int list) : int =
  let rec small_helper (l : int list) (found : bool) (n : int) : int =
    match l with
    | [] -> if found then n else raise Not_found
    | h::t -> if h < n && h > 0 then small_helper t true h else small_helper t found n
  in
  small_helper l false max_int

(* Q2 *)
let smallest_or_zero (l : int list) : int =
  try smallest l with Not_found -> 0
