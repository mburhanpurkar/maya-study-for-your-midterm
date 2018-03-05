type natural_num = One | OnePlus of natural_num

let rec double (num: natural_num) : natural_num =
  match num with
  | One -> OnePlus(One)
  | OnePlus n -> OnePlus(OnePlus(double n))

let rec natural_map (f: natural_num -> natural_num)
                    (num: natural_num) : natural_num =
  match num with
  | One -> f One
  | OnePlus n -> f (OnePlus(natural_map f n))

let rec collapse (n: natural_num) : int =
  match n with
  | One -> 1
  | OnePlus n -> 1 + (collapse n)

let rec fold (f: natural_num -> 'a -> 'a)
             (acc: 'a)
             (num: natural_num) : 'a =
  match num with
  | One -> acc
  | OnePlus n -> fold f (f (OnePlus n) acc) n

let collapse_fold (n: natural_num) : int = fold (fun _ acc -> acc + 1) 1 n

let is_even (n: natural_num) : bool = fold (fun _ acc -> not acc) false n

let div_two (n: natural_num) : natural_num option =
  match is_even n with
  | false -> None
  | true -> Some (fold (fun n acc -> if not (is_even n) then OnePlus(acc) else acc) One n)
