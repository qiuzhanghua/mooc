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
