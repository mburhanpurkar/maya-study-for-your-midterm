(*

OCaml from the Very Beginning Chapter 16 and Real World OCaml Chapter 9
Modules and Functors

The following is an attempt at making the make interval and the int and string
modules in Real World OCaml...

*)

(* First, make the comparable signature -- this will be kind of values we use to
   construct the interval module *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* Now, create the functor's signature that defines what parts of the functor will
   be accessible to the user *)
module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
end

(* Now, create the functor *)
module Make_interval (Endpoint : Comparable) : (Interval_intf with type endpoint = Endpoint.t) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  type endpoint = Endpoint.t

  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  let is_empty = ( = ) Empty

  let contains interval x =
    match interval with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >=0 && Endpoint.compare x h <= 0
end

(* Now, we can use it to create int and string intervals *)
module Int_interval = Make_interval(struct type t = int let compare = compare end);;
let i = Int_interval.create 3 4;;
