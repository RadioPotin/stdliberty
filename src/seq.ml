type 'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let empty () = Nil

let return x () = Cons (x, empty)

let cons x xs () = Cons (x, xs)

let rec append xs xy () =
  match xs() with
  | Nil -> xy()
  | Cons (x, r) -> Cons (x, append r xy)

let rec map f seq () =
  match seq() with
  | Nil -> Nil
  | Cons (x, r) -> Cons (f x, map f r)

let rec filter f seq () =
  match seq() with
  | Nil -> Nil
  | Cons (x, r) ->
    if f x then Cons (x, filter f r)
    else filter f r ()

let rec filter_map f seq () =
  match seq() with
  | Nil -> Nil
  | Cons (x, r) ->
    match f x with
    | None -> filter_map f r ()
    | Some thing -> Cons (thing, filter_map f r)

let rec flat_map f seq () =
  match seq() with
  | Nil -> Nil
  | Cons (x, r) ->
    append (f x) (flat_map f r) ()

let rec fold_left f acc seq =
    match seq() with
    | Nil -> acc
    | Cons (x, r) -> fold_left f (f acc x) r

let rec iter f seq () =
  match seq() with
  | Nil -> ()
  | Cons (x, r) ->
    f x;
    iter f r ()

let rec unfold f init () =
    match f init with
    | None -> Nil
    | Some (x, r) -> Cons (x, unfold f r)
