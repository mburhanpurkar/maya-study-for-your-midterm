(*

Helpful Operator Stuff
----------------------
&& has higher precedence than || i.e. a && b || c is the same as (a && b) || c
mod, / , * have higher precedence than -, +
they are left-associative i.e. a mod b mod c = (a mod b) mod c

*)

(*
Q1

17 : int
11 : int
1 : int
true : bool
false : bool
true : bool
false : bool
false : bool
'%' : char
TypeError

Q2
1 + 2 mod 3 = 1 + (2 mod 3) = 1 + 2 = 3
(1 + 2) mod 3 = 3 mod 3 = 0
=> mod is of higher precedence than +

Q3
1 + 2 * 3 + 4 = 11

Q4
min_int: int = -4611686018427387904
max_int: int = 4611686018427387903

Q5
1 / 0;;
Exception: Division_by_zero.

Q6
If the 1st operand of mod is 0, then 0
If the 2nd operand of mod is 0 then Exception: Division_by_zero
If the one of the operands of mod is negative, then the same output as both
being positive multiplied by -1

Q7
Weak typing!

Q8
Comparison operators on chars uses ascii values (uppercase is smaller than
lowercase)

*)
