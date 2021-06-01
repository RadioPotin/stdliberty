type 'a t = 'a array

(* Array operations *)

external length : 'a array -> int = "%array_length"
external get: 'a array -> int -> 'a = "%array_safe_get"
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external make: int -> 'a -> 'a array = "caml_make_vect"
external create: int -> 'a -> 'a array = "caml_make_vect"
external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external concat : 'a array list -> 'a array = "caml_array_concat"
external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"
external unsafe_fill :
  'a array -> int -> int -> 'a -> unit = "caml_array_fill"
external create_float: int -> float array = "caml_make_float_vect"
let make_float = create_float

let init l f =
  if l = 0 then [||]
  else if l < 0 || l > Sys.max_array_length then invalid_arg "init"
    else
      let a = create l (f 0) in
      for i = 1 to pred l do
        unsafe_set a i (f i)
      done;
      a

let make_matrix dx dy v =
  let matrix = create dx [||] in
  for i = 0 to pred dx do
    unsafe_set matrix i (create dy v)
  done;
  matrix

let copy a =
   let l = length a in
   if l = 0 then [||] else unsafe_sub a 0 l

let append a1 a2 =
  let l1 = length a1 in
  let l2 = length a2 in
  let len = l1 + l2 in
  if len > Sys.max_array_length then invalid_arg "append"
  else if l1 = 0 then copy a2
  else if l2 = 0 then copy a1
  else
    append_prim a1 a2

let sub a pos len =
  if len < 0 || pos < 0 || len + pos > length a then invalid_arg "sub"
  else
    unsafe_sub a pos len

let fill a pos len x =
  if len < 0 || pos < 0 || len + pos > length a then invalid_arg "fill"
  else
    unsafe_fill a pos len x

let blit a1 pos1 a2 pos2 len =
  if len < 0 || pos1 < 0 || len + pos1 > length a1 || pos2 < 0 || len + pos2 > length a2  then invalid_arg "blit"
  else
    unsafe_blit a1 pos1 a2 pos2 len

let to_list =
  let rec to_list_aux a l n =
    if n < 0 then l else to_list_aux a ((unsafe_get a n)::l) (n - 1)
  in fun a -> to_list_aux a [] (pred (length a))

let of_list =
  let rec of_list_aux a i = function
    | [] -> a
    | x::r ->
      unsafe_set a i x;
      of_list_aux a (i + 1) r
in fun l ->
  let len = List.length l in if len > Sys.max_array_length then invalid_arg "of_list"
      else if l = [] then of_list_aux [||] 0 []
      else let a = create len (List.hd l) in of_list_aux a 0 l

let iter f =
  let rec iter_aux a = function
    | 0 -> f (unsafe_get a 0); ()
    | n -> f (unsafe_get a n); iter_aux a (n - 1)
in fun a -> iter_aux a ((length a) - 1)

let iteri f =
  let rec iteri_aux a = function
    | 0 -> f 0 (unsafe_get a 0); ()
    | n -> f n (unsafe_get a n); iteri_aux a (n - 1)
in fun a -> iteri_aux a ((length a) - 1)

let map f =
  let rec map_aux a acc = function
    | 0 -> unsafe_set acc 0 (f (unsafe_get a 0)); acc
    | n -> unsafe_set acc n (f (unsafe_get a n)); map_aux a acc (n - 1)
in fun a ->
  let len = (length a) in
  let acc = create len (f (unsafe_get a (len - 1))) in
  map_aux a acc (len - 2)

let mapi f =
  let rec mapi_aux a acc = function
    | 0 -> unsafe_set acc 0 (f 0 (unsafe_get a 0)); acc
    | n -> unsafe_set acc n (f n (unsafe_get a n)); mapi_aux a acc (n - 1)
in fun a ->
  let len = (length a) in
  let acc = create len (f (len - 1) (unsafe_get a (len - 1))) in
  mapi_aux a acc (len - 2)

(*
 mapi, fold_left, fold_right, iter2, map2, for_all, exists, for_all2, exists2, mem, memq, sort, stable_sort, fast_sort, to_seq, to_seqi, of_seq
 *)
