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
    |x::r -> f x (fold_right_aux f acc r)
  in fun f l acc -> fold_right_aux f acc l

let map2 f =
  let rec map2_aux l1 l2 =
    match (l1, l2) with
    | ([], []) -> []
    | (x1::r1, x2::r2)  -> (f x1 x2)::(map2_aux r1 r2)
    | _ -> invalid_arg "map2"
  in fun l1 l2 ->
      rev (map2_aux l1 l2)

let rev_map2 f l1 l2 = rev (map2 f l1 l2)

(* iter2 fold_left2 fold_right2 for_all exists for_all2 exists2 mem memq assoc assoc_opt assq assq_opt mem_assoc mem_assq remove_assoc remove_assq find (= find_opt) find_exn find_map find_all (=filter) filteri filter_map concat_map fold_left_map partition partition_map split combine merge compare_lengths compare_length_with equal compare to_seq of_seq *)
