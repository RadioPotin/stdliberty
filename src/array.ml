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

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let map f a =
  let len = length a in
  if len = 0 then [||]
  else
    let arr = create len (f(unsafe_get a 0)) in
    for i = 1 to len - 1 do
      unsafe_set arr i (f(unsafe_get a i))
    done;
    arr

let mapi f a =
  let len = length a in
  if len = 0 then [||]
  else
    let arr = create len (f 0 (unsafe_get a 0)) in
    for i = 1 to len - 1 do
    unsafe_set arr i (f i (unsafe_get a i))
  done;
    arr

let fold_left f init a =
  let r = ref init in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_right f a init =
  let r = ref init in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

  let iter2 f a b =
    let len = length a in
    if len <> length b then invalid_arg "iter2"
    else
      for i = 0 to len - 1 do
        f (unsafe_get a i) (unsafe_get b i)
        done

let map2 f a b =
  let len = length a in
  if len <> length b then invalid_arg "map2"
  else if len = 0 then [||]
  else
    let arr = create len (f (unsafe_get a 0) (unsafe_get b 0)) in
    for i = 1 to len - 1 do
      unsafe_set arr i (f (unsafe_get a i) (unsafe_get b i))
    done;
    arr

let for_all f a =
  let len = length a in
  let rec scan i =
    if i = len then true
    else if f (unsafe_get a i) then scan (i + 1)
    else false in
  scan 0

let exists f a =
  let len = length a in
  let rec scan i =
    if i = len then false
    else if f (unsafe_get a i) then true
    else scan (i + 1) in
  scan 0

let for_all2 f a b =
  let len = length a in
  if len <> length b then invalid_arg "for_all2"
  else
  let rec scan i =
    if i = len then true
    else if f (unsafe_get a i) (unsafe_get b i) then scan (i - 1)
    else false in
  scan 0

let exists2 f a b =
  let len = length a in
  if len <> length b then invalid_arg "for_all2"
  else
  let rec scan i =
    if i = len then false
    else if f (unsafe_get a i) (unsafe_get b i) then true
    else
      scan (i + 1) in
  scan 0

let mem el a =
  let len = length a in
  let rec scan i =
    if i = len then false
    else if compare el a = 0 then true
    else scan (i + 1)
  in scan 0

let memq el a =
  let len = length a in
  let rec scan i =
    if i = len then false
    else if el == (unsafe_get a i) then true
    else scan (i + 1)
  in scan 0

let to_seq a =
  let rec to_seq_aux i () =
    if i < length a then
      let el = unsafe_get a i in
      Seq.Cons (el, to_seq_aux (i + 1))
    else Seq.Nil
  in to_seq_aux 0

let to_seqi a =
  let rec to_seqi_aux i () =
    if i < length a then
      let el = unsafe_get a i in
      Seq.Cons ((el, i), to_seqi_aux (i + 1))
    else Seq.Nil
  in to_seqi_aux 0

let of_rev_list = function
  | [] -> [||]
  | x::r as l ->
    let len = List.length l in
    let arr = create len x in
    let rec fill i = function
      | [] -> arr
      | x::r -> unsafe_set arr i x; fill (i + 1) r
    in
    fill (len - 2) r

let of_seq i =
  let l = Seq.fold_left (fun acc x -> x::acc) [] i in of_rev_list l
