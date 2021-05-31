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

let append a1 a2 =
  let l1 = length a1 in
  let l2 = length a2 in
  let len = l1 + l2 in
  if len > Sys.max_array_length then invalid_arg "append"
  else if l1 = 0 then a2
  else if l2 = 0 then a1
  else
    append_prim a1 a2

let sub a pos len =
  if len < 0 || pos < 0 || len + pos > length a then invalid_arg "sub"
  else
    unsafe_sub a pos len


(*
 sub, copy, fill, blit, to_list, of_list, iter, iteri, map, mapi, fold_left, fold_right, iter2, map2, for_all, exists, for_all2, exists2, mem, memq, sort, stable_sort, fast_sort, to_seq, to_seqi, of_seq
 *)
