type 'a t = 'a list =
  | []
  | (::) of 'a * 'a t

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
    | x::r -> flatten_aux (x::acc) r
      in fun l -> rev (flatten_aux [] l)

let map f =
  let rec map_aux f acc = function
    | [] -> acc
    | x::r -> map_aux f ((f x)::acc) r
  in fun l -> rev (map_aux f [] l)

let mapi =
  let rec mapi_aux f i acc = function
    | [] -> acc
    | x::r -> mapi_aux f (i + 1) ((f i x)::acc) r
  in fun f l -> rev (mapi_aux f 0 [] l)

let rev_map f l = rev (map f l)

let iter =
  let rec iter_aux f = function
    | [] -> ()
    | x::r -> f x; iter_aux f r
in fun f l -> iter_aux f l

let iteri =
  let rec iteri_aux f i = function
    | [] -> ()
    | x::r -> f i x; iteri_aux f (i + 1) r
in fun f l -> iteri_aux f 0 l

let fold_left =
  let rec fold_left_aux f acc = function
    | [] -> acc
    | x::r -> fold_left_aux f (f acc x) r
  in fun f l -> fold_left_aux f [] l

let fold_right =
  let rec fold_right_aux f acc = function
    | [] -> acc
    | x::r -> f x (fold_right_aux f acc r)
  in fun f l acc -> fold_right_aux f acc l

let map2 f =
  let rec map2_aux = function
    | [], [] -> []
    | x1::r1, x2::r2  -> (f x1 x2)::(map2_aux (r1, r2))
    | _ -> invalid_arg "map2"
  in fun l1 l2 ->
      rev (map2_aux (l1, l2))

let rev_map2 f l1 l2 = rev (map2 f l1 l2)

let iter2 =
  let rec iter2_aux f = function
    | [], [] -> ()
    | x1::r1, x2::r2 -> f x1 x2; iter2_aux f (r1, r2)
    | _ -> invalid_arg "iter2"
in fun f l1 l2 -> iter2_aux f (l1, l2)

let fold_left2 =
  let rec fold_left2_aux f acc = function
    | [], [] -> acc
    | x1::r1, x2::r2 -> fold_left2_aux f (f acc x1 x2) (r1, r2)
    | _ -> invalid_arg "fold_left2"
  in fun f l1 l2 -> fold_left2_aux f [] (l1, l2)

let fold_right2 =
  let rec fold_right2_aux f acc = function
    | [], [] -> acc
    | x1::r1, x2::r2 -> f x1 x2 (fold_right2_aux f acc (r1, r2))
    | _ -> invalid_arg "fold_right2"
  in fun f l1 l2 acc -> fold_right2_aux f acc (l1, l2)

let for_all f =
  let rec for_all_aux = function
    | [] -> true
    | x::r -> if f x then for_all_aux r else false
  in fun l -> for_all_aux l

let exists f =
  let rec exists_aux = function
    | [] -> false
    | x::r -> if f x then true else exists_aux r
  in fun l -> exists_aux l

let for_all2 f =
  let rec for_all2_aux = function
    | [], [] -> true
    | x1::r1, x2::r2 -> if f x1 x2 then for_all2_aux (r1, r2) else false
    | _ -> invalid_arg "for_all2"
  in fun l1 l2 -> for_all2_aux (l1, l2)

let exists2 f =
  let rec exists2_aux = function
    | [], [] -> false
    | x1::r1, x2::r2 -> if f x1 x2 then true else exists2_aux (r1, r2)
    | _ -> invalid_arg "exists2"
  in fun l1 l2 -> exists2_aux (l1, l2)

let mem =
  let rec memaux a = function
    | [] -> false
    | x::r -> if a = x then true else memaux a r
in fun a set -> memaux a set

let memq =
  let rec memqaux a = function
    | [] -> false
    | x::r -> if a == x then true else memqaux a r
in fun a set -> memqaux a set

let assoc =
  let rec assoc_aux a = function
    | [] -> Not_found
    | x::r -> if fst(x) = a then snd(x) else assoc_aux a r
  in fun a l -> assoc_aux a l

let assoc_opt =
  let rec assoc_opt_aux a = function
    | [] -> None
    | x::r -> if fst(x) = a then snd(x) else assoc_opt_aux a r
  in fun a l -> assoc_opt_aux a l

let assq =
  let rec assq_aux a = function
    | [] -> Not_found
    | x::r -> if fst(x) == a then snd(x) else assq_aux a r
  in fun a l -> assq_aux a l

let assq_opt =
  let rec assq_opt_aux a = function
    | [] -> None
    | x::r -> if fst(x) == a then snd(x) else assq_opt_aux a r
  in fun a l -> assq_opt_aux a l

let mem_assoc =
  let rec mem_assoc_aux a = function
    | [] -> false
    | x::r -> if fst(x) = a then true else mem_assoc_aux a r
  in fun a l -> mem_assoc_aux a l

let mem_assq =
  let rec mem_assq_aux a = function
    | [] -> false
    | x::r -> if fst(x) == a then true else mem_assq_aux a r
  in fun a l -> mem_assq_aux a l

let remove_assoc =
  let rec remove_assoc_aux a = function
  | [] -> []
  | x::l -> if fst(x) = a then l else x::(remove_assoc_aux a l)
  in fun a l -> remove_assoc_aux a l

let remove_assocq =
  let rec remove_assocq_aux a = function
  | [] -> []
  | x::l -> if fst(x) == a then l else x::(remove_assocq_aux a l)
  in fun a l -> remove_assocq_aux a l

let find_opt f =
  let rec find_opt_aux = function
    | [] -> None
    | x::r -> if f x then Some x else find_opt_aux r
  in fun l -> find_opt_aux l

let find = find_opt

let find_exn f =
  let rec find_exn_aux = function
    | [] -> Not_found
    | x::r -> if f x then x else find_exn_aux r
  in fun l -> find_exn_aux l



(* find_map find_all (=filter) filteri filter_map concat_map fold_left_map partition partition_map split combine merge compare_lengths compare_length_with equal compare to_seq of_seq *)
