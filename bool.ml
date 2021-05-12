type t = bool =
  | false
  | true

let not = function
  | true -> false
  | false -> true

let (&&) a b =
  if a then b else false

let (||) a b =
  if a then true else b

let equal = function
  | true, true | false, false -> true
  | _b1, _b2 -> false

let compare = function
  | true, true | false, false -> 0
  | false, true -> -1
  | true, false -> 1

let to_int a =
  if a then 1 else 0

let to_float a =
  if a then 1. else 0.

let to_string a =
  if a then "true" else "false"

let of_string_opt = function
  | "false" -> Some false
  | "true" -> Some true
  | _other -> None

let of_string_exn = function
  | "true" -> true
  | "false" -> false
  | _other -> failwith "halp"

let of_string a =
  of_string_opt a
