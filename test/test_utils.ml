let string_of_triple (a, b, c) = Printf.sprintf "(%d, %d, %d)" a b c
let string_of_pair (a, b) = Printf.sprintf "(%d, %d)" a b

let string_of_list list =
  let string_list = List.map string_of_int list in
  String.concat "; " string_list

let string_of_list_pair (a, b) =
  Printf.sprintf "(%s, %s)" (string_of_list a) (string_of_list b)
