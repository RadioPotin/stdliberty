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
  | [] -> raise (Invalid_argument "Invalid_argument")
  | _x::r -> r

let nth_opt l i =
  if i < 0 then
    raise (Invalid_argument "nth_opt")
  else
    let rec nth_aux i = function
      | [] -> None
      | x::r -> if i = 0 then Some x else nth_aux (i - 1) r
    in nth_aux i l

let nth_exn l i =
  if i < 0 then
    raise (Invalid_argument "nth_exn")
  else
    let rec nth_aux i = function
      | [] -> failwith "nth_exn"
      | x::r -> if i = 0 then x else nth_aux (i - 1) r
    in nth_aux i l

let nth = nth_opt

(* append rev_append rev init flatten map mapi rev_map iter iteri fold_left fold_right map2 rev_map2 iter2 fold_left2 fold_right2 for_all exists for_all2 exists2 mem memq assoc assoc_opt assq assq_opt mem_assoc mem_assq remove_assoc remove_assq find (= find_opt) find_exn find_map find_all (=filter) filteri filter_map concat_map fold_left_map partition partition_map split combine merge compare_lengths compare_length_with equal compare to_seq of_seq *)
