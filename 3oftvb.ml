(* Pattern Matching *)

(* Q1 *)
let not (x : bool) =
  match x with
  | true -> false
  | false -> true

(* Q2 *)
let rec sum2 (x : int) =
  match x with
  | 0 -> 0
  | a -> a + sum2 (a - 1)

(* Q3 *)
let rec pow2 (x : int) (n : int) =
  match n with
  | 0 -> 1
  | b -> x * pow2 x (n - 1)

(* Q4 - This is silly *)

(* Q5
   match 1 + 1 with 2 -> match 2 + 2 with 3 -> 4 | 4 -> 5
   5 : int
*)

(* Q6 *)
let is_lower (c : char) =
  match c with
  | 'a'..'z' -> true
  | _ -> false

let is_upper (c : char) = not (is_lower c)
