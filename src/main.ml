(* open Stdliberty *)

let print_x fmt x =
  Format.fprintf fmt "%d, " x

let rec print_list_el fmt = function
  | [] -> Format.fprintf fmt "END"
  | x::r -> print_x fmt x; print_list_el fmt r

let print_list_list l =
  let fmt = Format.std_formatter in
  match l with
  | [] -> ()
  | l -> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") print_list_el fmt l

let () =
  let l1 = [[0;1;2;3;4];[5;6;7;8;9];[10;11;12;13;14];[]] in
  Stdliberty.List.iteri (fun i x -> if i = 3 then print_int (x * 2) else ()) (Stdliberty.List.hd l1)
