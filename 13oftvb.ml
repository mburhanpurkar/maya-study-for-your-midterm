(*
References:
  Initialize a reference ('a -> 'a ref) using let x = ref 0;;
  Extract a reference ('a ref -> 'a) using let p = !x;;
  Update a reference ('a ref -> 'a -> ()) using x := 50;;
*)

let swap a b =
  let t = !a in
  a := !b; b := t

(*
We may omit the else in if... then... else if it would just be (), but must
either use brackets or "begin" and "end" to specify bodies of if and else
statements:
  if x = y then
    (a := !a + 1;
     b := !b + 1)
  else
    c := !c + 1

OR
if x = y then
  begin
    a := !a + 1;
    b := !b + 1
  end
else
  c := !c + 1
*)

(*
We have also become civilized enough to use for loops now:
  for x = 1 to 5 do print_int x; print_newline () done;

We can also do while loops:
*)

let smallest_pow2 x =
  let t = ref 1 in
  while !t < x do
    t := !t * 2
  done;
  !t

(* Practice Problems: *)

(* Q1
let x = ref 1 in
let y = ref 2 in
x := !x + !x;
y := !x + !y;
!x + !y
=> 6 *)

(* Q2 - if we update the value of x, both in the second list change *)

(* Q3 - example print 1 to 5 *)
let rec conditional_print (x : int) : unit =
  if x <= 5 then
    begin
      print_int x;
      print_newline ();
      conditional_print (x + 1)
    end
;;
conditional_print 1

(* Q4
   a) int array = [|1; 2; 3|] (note, we can do subscripting and assignment a.(0) <- 4 )
   b) bool array
   c) int array array
   d) int list array
   e) int
   f) ()
*)

(* Q5 *)
let sum_array (x : int array) : int =
  let sum = ref 0 in
  for y = 0 to Array.length x - 1 do sum := !sum  + x.(y) done;
  !sum

(* Q6 *)
let rev_array (a : 'a array) : 'a array =
  if a <> [||] then
    for x = 0 to int_of_float (ceil (float_of_int (Array.length a) /. 2. -. 1.)) do
      let t = a.(x) in
      a.(x) <- a.(Array.length a - 1 - x);
      a.(Array.length a - 1 - x) <- t;
    done; a

(* Q7 *)
let gen_mults (x : int) : int array array =
  (* ?????? OCAML WHYYYYYY - ASK JEFFREY!!! *)
  (* let a = Array.make x (Array.make x 0) in *)
  let a = Array.make x [||] in
  for b = 0 to x - 1 do
    a.(b) <- Array.make x 0
  done;
  for row = 0 to x - 1 do
    for col = 0 to x - 1 do
      a.(row).(col) <- (row + 1) * (col + 1)
    done;
  done;
  a
