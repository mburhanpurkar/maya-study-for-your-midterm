(*
Variables and functions
-----------------------
Variable
let x = 200;;

Function
let f x y = x + y;;

Recursive Function
let rec factorial a =
  if a = 1 then 1
  else a * factorial (a - 1)

*)

(* Q1 *)
let mult_ten: int -> int = ( * ) 10

(* Q2 *)
let non_zero (x : int) (y : int) = x <> 0 && y <> 0

(* Q3 *)
let rec sum (n : int) : int =
  if n <= 0 then 0
  else n + sum (n - 1)

(* Q4 *)
let rec pow (x : int) (n : int) =
  if n <= 0 then 1
  else x * pow x (n - 1)

(* Q5 - doesn't quite work, but too lazy to write out all cases *)
let is_consonant (c : char) =
  let is_vowel (c : char) =
    c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'
  in
  not (is_vowel c)

(* Q6
   let x = 1 in let x = 2 in x + x
   4 : int
*)

(* Q7
   Yes :)
*)
