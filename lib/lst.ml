let sum l = List.fold_left ( + ) 0 l

let count_even l =
  List.fold_left (fun acc x -> if x mod 2 = 0 then acc + 1 else acc) 0 l

let palindrome l = List.rev l = l
let uppercase l = List.map Char.uppercase_ascii l

let is_sorted l f =
  let rec aux l f =
    match l with
    | [] | [ _ ] -> true
    | x :: y :: tl -> if f x y < 0 then aux (y :: tl) f else false
  in
  aux l f

let remove_duplicate l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x :: tl -> if List.mem x acc then aux tl acc else aux tl (x :: acc)
  in
  aux l []

let remove_duplicate_sorted l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | [ x ] -> x :: acc
    | x :: y :: tl ->
        if x = y then aux (y :: tl) acc else aux (y :: tl) (x :: acc)
  in
  aux l []

(* let halve2 l =
   let (len, rev) = List.fold_left (fun (l, r) x -> (l+1, x::r)) (0, []) l in
   let l2 = (len + 1) / 2 in
   let l1 = len - l2 in
   let first_half = l |> List.to_seq |> Seq.take l1 |> List.of_seq in
   let second_half = rev |> List.to_seq |> Seq.take l2 |> List.of_seq |> List.rev in
   (first_half, second_half) *)

let halve lst =
  let cnt = ref 0 in
  let acc = ref ([], []) in
  let rec aux c l =
    match l with
    | [] -> cnt := c / 2
    | hd :: tl ->
        aux (c + 1) tl;
        let f, s = !acc in
        if c < !cnt then acc := (hd :: f, s) else acc := (f, hd :: s)
  in
  aux 0 lst;
  !acc
