type 'a t = 'a list =
  | []
  | (::) of 'a * 'a t

let notfound () = raise Not_found

let length =
  let rec len_aux acc = function
  | [] -> acc
  | _x::s -> len_aux (acc + 1) s
  in
  fun l -> len_aux 0 l

let cons x l = x::l

let hd = function
  | [] -> failwith "hd"
  | x::_r -> x

let tl = function
  | [] -> invalid_arg "Invalid_argument"
  | _x::r -> r

let nth_opt l i =
  if i < 0 then
    invalid_arg "nth_opt"
  else
    let rec nth_aux i = function
      | [] -> None
      | x::r -> if i = 0 then Some x else nth_aux (i - 1) r
    in nth_aux i l

let nth_exn l i =
  if i < 0 then
    invalid_arg "nth_exn"
  else
    let rec nth_aux i = function
      | [] -> failwith "nth_exn"
      | x::r -> if i = 0 then x else nth_aux (i - 1) r
    in nth_aux i l

let nth = nth_opt

let append = (@)

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x::r -> rev_append r (x::l2)

let rev l = rev_append l []

let init =
    let rec init_aux acc i f len =
      if i >= len then acc
      else
        init_aux ((f i)::acc) (i + 1) f len
    in fun len f ->
      if len < 0 then
        invalid_arg "init"
      else
        rev (init_aux [] 0 f len)

let flatten =
  let rec flatten_aux acc = function
    | [] -> acc
    | x::r -> flatten_aux (x @ acc) r
      in fun l -> rev (flatten_aux [] l)

let map f =
  let rec map_aux acc = function
    | [] -> acc
    | x::r -> map_aux ((f x)::acc) r
  in fun l -> rev (map_aux [] l)

let mapi f =
  let rec mapi_aux i acc = function
    | [] -> acc
    | x::r -> mapi_aux (i + 1) ((f i x)::acc) r
  in fun l -> rev (mapi_aux 0 [] l)

let rev_map f =
  let rec map_aux acc = function
    | [] -> acc
    | x::r -> map_aux ((f x)::acc) r
  in fun l -> map_aux [] l

let iter f =
  let rec iter_aux = function
    | [] -> ()
    | x::r -> f x; iter_aux r
in fun l -> iter_aux l

let iteri f =
  let rec iteri_aux i = function
    | [] -> ()
    | x::r -> f i x; iteri_aux (i + 1) r
in fun l -> iteri_aux 0 l

let fold_left f =
  let rec fold_left_aux acc = function
    | [] -> acc
    | x::r -> fold_left_aux (f acc x) r
  in fun l -> fold_left_aux [] l

let fold_right f =
  let rec fold_right_aux acc = function
    | [] -> acc
    | x::r -> f x (fold_right_aux acc r)
  in fun l acc -> fold_right_aux acc l

let map2 f =
  let rec map2_aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | x1::r1, x2::r2  -> map2_aux ((f x1 x2)::acc) r1 r2
    | _ -> invalid_arg "map2"
  in fun l1 l2 ->
    rev (map2_aux [] l1 l2)

let rev_map2 f =
  let rec revmap2_aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | x1::r1, x2::r2  -> revmap2_aux ((f x1 x2)::acc) r1 r2
    | _ -> invalid_arg "revmap2"
  in fun l1 l2 ->
    revmap2_aux [] l1 l2

let iter2 f =
  let rec iter2_aux l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | x1::r1, x2::r2 -> f x1 x2; iter2_aux r1 r2
    | _ -> invalid_arg "iter2"
in fun l1 l2 -> iter2_aux l1 l2

let fold_left2 f =
  let rec fold_left2_aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | x1::r1, x2::r2 -> fold_left2_aux (f acc x1 x2) r1 r2
    | _ -> invalid_arg "fold_left2"
  in fun l1 l2 -> fold_left2_aux [] l1 l2

let fold_right2 f =
  let rec fold_right2_aux l1 l2 acc =
    match l1, l2 with
    | [], [] -> acc
    | x1::r1, x2::r2 -> f x1 x2 (fold_right2_aux r1 r2 acc)
    | _ -> invalid_arg "fold_right2"
  in fun l1 l2 acc -> fold_right2_aux l1 l2 acc

let for_all f =
  let rec for_all_aux = function
    | [] -> true
    | x::r -> f x && for_all_aux r
  in fun l -> for_all_aux l

let exists f =
  let rec exists_aux = function
    | [] -> false
    | x::r -> f x || exists_aux r
  in fun l -> exists_aux l

let for_all2 f =
  let rec for_all2_aux l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | x1::r1, x2::r2 -> f x1 x2 && for_all2_aux r1 r2
    | _ -> invalid_arg "for_all2"
  in fun l1 l2 -> for_all2_aux l1 l2

let exists2 f =
  let rec exists2_aux l1 l2 =
    match l1, l2 with
    | [], [] -> false
    | x1::r1, x2::r2 ->  f x1 x2 || exists2_aux r1 r2
    | _ -> invalid_arg "exists2"
  in fun l1 l2 -> exists2_aux l1 l2

let mem a =
  let rec memaux = function
    | [] -> false
    | x::r -> a = x || memaux r
in fun set -> memaux set

let memq a =
  let rec memqaux = function
    | [] -> false
    | x::r -> a == x || memqaux r
in fun set -> memqaux set

let assoc a =
  let rec assoc_aux = function
    | [] -> notfound ()
    | x::r ->
      match x with
      | (xl, xr) -> if xl = a then xr else assoc_aux r
  in fun l -> assoc_aux l

let assoc_opt a =
  let rec assoc_opt_aux = function
    | [] -> None
    | x::r ->
      match x with
      | (xl, xr) -> if xl = a then Some xr else assoc_opt_aux r
  in fun l -> assoc_opt_aux l

let assq a =
  let rec assq_aux = function
    | [] -> notfound ()
    | x::r ->
      match x with
      | (xl, xr) -> if xl == a then xr else assq_aux r
  in fun l -> assq_aux l

let assq_opt a =
  let rec assq_opt_aux = function
    | [] -> None
    | x::r ->
      match x with
      | (xl, xr) -> if xl == a then Some xr else assq_opt_aux r
  in fun l -> assq_opt_aux l

let mem_assoc a =
  let rec mem_assoc_aux = function
    | [] -> false
    | x::r ->
      match x with
      | (xl, _xr) -> xl = a || mem_assoc_aux r
  in fun l -> mem_assoc_aux l

let mem_assq a =
  let rec mem_assq_aux = function
    | [] -> false
    | x::r ->
      match x with
      | (xl, _xr) -> xl == a || mem_assq_aux r
  in fun l -> mem_assq_aux l

let remove_assoc a =
  let rec remove_assoc_aux acc = function
  | [] -> acc
  | (xl, _ as pair)::l -> if xl = a then l else remove_assoc_aux (pair::acc) l
in fun l -> rev (remove_assoc_aux [] l)

let remove_assocq a =
  let rec remove_assocq_aux acc = function
  | [] -> acc
  | (xl, _ as pair)::l -> if xl = a then l else remove_assocq_aux (pair::acc) l
in fun l -> rev (remove_assocq_aux [] l)

let find_opt f =
  let rec find_opt_aux = function
    | [] -> None
    | x::r -> if f x then Some x else find_opt_aux r
  in fun l -> find_opt_aux l

let find = find_opt

let find_exn f =
  let rec find_exn_aux = function
    | [] -> notfound ()
    | x::r -> if f x then x else find_exn_aux r
  in fun l -> find_exn_aux l

let find_map f =
  let rec find_map_aux = function
    | [] -> None
    | x::r ->
      match f x with
      | None -> find_map_aux r
      | Some _ as b -> b
  in fun l -> find_map_aux l

let find_all f =
  let rec find_all_aux acc = function
    | [] -> rev acc
    | x::r -> if f x then find_all_aux (x::acc) r else find_all_aux acc r
  in fun l -> find_all_aux [] l

let filter = find_all

let filteri f =
  let rec filteri_aux i acc = function
    | [] -> rev acc
    | x::r ->
      if f i x then
        filteri_aux (i + 1) (x::acc) r
      else
        filteri_aux (i + 1) acc r
  in fun l -> filteri_aux 0 [] l

let filter_map f =
  let rec filter_map_aux acc = function
    | [] -> rev acc
    | x::r ->
      match f x with
      | None -> filter_map_aux acc r
      | Some value -> filter_map_aux (value::acc) r
  in fun l -> filter_map_aux [] l

let partition f =
  let rec parti_aux satisf disatisf = function
    | [] -> (rev satisf, rev disatisf)
    | x::r ->
      if f x then
        parti_aux (x::satisf) disatisf r
      else
        parti_aux satisf (x::disatisf) r
  in fun l -> parti_aux [] [] l

let partition_map f =
  let rec parti_map_aux satisf disatisf = function
    | [] -> (rev satisf, rev disatisf)
    | x::r ->
      match f x with
      | Either.Left x -> parti_map_aux (x::satisf) disatisf r
      | Either.Right x -> parti_map_aux satisf (x::disatisf) r
  in fun l -> parti_map_aux [] [] l

let concat_map f =
  let rec concat_map_aux acc = function
    | [] -> rev acc
    | x::r -> concat_map_aux (rev_append (f x) acc) r
  in fun l -> concat_map_aux [] l

let split =
  let rec split_aux accl accr = function
    | [] -> (rev accl, rev accr)
    | (xl, xr)::r -> split_aux (xl::accl) (xr::accr) r
  in fun l -> split_aux [] [] l

let combine =
  let rec combine_aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> rev acc
    | x1::r1, x2::r2 -> combine_aux ((x1, x2)::acc) r1 r2
    | _ -> invalid_arg "combine"
  in fun l1 l2 -> combine_aux [] l1 l2

let merge cmp =
  let rec merge_aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> rev acc
    | l1, [] -> l1
    | [], l2 -> l2
    | x1::r1, x2::r2 ->
      if cmp x1 x2 <= 0 then
        merge_aux (x1::acc) r1 l2
      else
        merge_aux (x2::acc) l1 r2
  in fun l1 l2 -> merge_aux [] l1 l2

let compare f =
  let rec compare_aux l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | hd1::tl1, hd2::tl2 ->
      if f hd1 hd2 = 0 then
        compare_aux tl1 tl2
      else f hd1 hd2
  in fun l1 l2 -> compare_aux l1 l2

let equal eq =
  let rec equal_aux l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | x1::r1, x2::r2 -> eq x1 x2 && equal_aux r1 r2
    | _, _ -> false
  in fun l1 l2 -> equal_aux l1 l2

let rec compare_lengths l1 l2=
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | _::r1, _::r2 -> compare_lengths r1 r2

let rec compare_length_with l len =
  match l with
    | [] -> if len = 0 then 0
    else if len > 0 then -1
    else 1
    | _::r -> compare_length_with r (len - 1)

(* fold_left_map to_seq of_seq *)
