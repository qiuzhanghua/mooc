open Mooc

let () =
  print_int (Ex01.power8 2);
  print_endline ""

let () = print_endline (Ex02.concat9 "Hi")

let () =
  print_float (Math.distance2 (1., 1.) (2., 2.));
  print_endline ""

let () =
  print_float (Math.area 2.0);
  print_endline ""

let () =
  print_float (Math.sin2x (Float.pi /. 4.0));
  print_endline ""

let p : Math.point = { x = 1.0; y = 2.0; z = 3.0 }
let dp : Math.dpoint = { dx = 1.0; dy = 2.0; dz = 3.0 }
let p' = Math.move p dp

let () =
  print_float p'.x;
  print_endline "";
  print_float p'.y;
  print_endline "";
  print_float p'.z;
  print_endline ""

let () =
  let o1 : Math.physical_object =
    {
      position = { x = 0.0; y = 0.0; z = 0.0 };
      velocity = { dx = 0.0; dy = 0.0; dz = 0.0 };
    }
  in
  let o2 : Math.physical_object =
    {
      position = { x = 3.1; y = 0.0; z = 0.0 };
      velocity = { dx = -1.0; dy = 0.0; dz = 0.0 };
    }
  in
  if Math.will_collide_soon o1 o2 then print_endline "collide"
  else print_endline "not cliide"

let () =
  Printf.printf "manhattan_distance of (0, 0) and (1, 1) is %d\n"
    (Math.manhattan_distance 0 0 1 1)

(* let sort1 a = Array.sort (fun x y -> compare y x) a

   let compare1 x y =
       if x mod 2 = 0 && y mod 2 = 0 then compare x y
       else if x mod 2 = 0 then -1
       else if y mod 2 = 0 then 1
       else compare x y

     let sort2 l = List.sort compare1 l
     let sort3 a = Array.sort compare1 a *)

let rec compare2 x y =
  if x < 10 && y < 10 then compare x y
  else if y = 0 then 1
  else
    let x' = x mod 10 in
    let y' = y mod 10 in
    if x' = y' then compare2 (x / 10) (y / 10) else compare x' y'

let sort4 l = List.sort compare2 l

let string_of_list list =
  let string_list = List.map string_of_int list in
  "[" ^ String.concat "; " string_list ^ "]"


let () = print_endline (string_of_list (sort4 [ 121; 17; 191; 32; 19; 91 ]))
