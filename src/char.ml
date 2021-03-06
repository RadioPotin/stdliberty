type t = char


let unsafe_chr : int -> char = Obj.magic

let code : char -> int = Obj.magic

let chr ascii = if 0 <= ascii && ascii <= 255 then unsafe_chr ascii else raise (Invalid_argument "Invalid_argument")

let escaped = function
  | '\'' -> "\'"
  | '\\' -> "\\"
  | '\t' -> "\\t"
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | ' ' .. '~' as c ->
    let s = Bytes.create 1 in Bytes.unsafe_set s 0 c; Bytes.unsafe_to_string s
  | c ->
    let n = code c
        in let s = Bytes.create 4
               in Bytes.unsafe_set s 0 '\\';
               Bytes.unsafe_set s 1 (unsafe_chr (48 + n / 100));
               Bytes.unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
               Bytes.unsafe_set s 3 (unsafe_chr (48 + n mod 10));
               Bytes.unsafe_to_string s

let lowercase_ascii = function
  | 'A'..'Z' as c -> unsafe_chr(code c + 32)
  | c -> c

let uppercase_ascii = function
  | 'a'..'z' as c -> unsafe_chr(code c - 32)
  | c -> c

let compare c1 c2 =
  code c1 - code c2

let equal c1 c2 =
  compare c1 c2 = 0
