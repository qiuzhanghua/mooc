let distance2 p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))

let area r = r *. r *. Float.pi

let sin2x x =
  let t = tan x in
  2.0 *. t /. (1. +. (t *. t))

type point = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp = { x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz }

let distance3 p1 p2 =
  let { x = x1; y = y1; z = z1 } = p1 in
  let { x = x2; y = y2; z = z2 } = p2 in
  sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.) +. ((z1 -. z2) ** 2.))

let will_collide_soon p1 p2 =
  let p1' = move p1.position p1.velocity in
  let p2' = move p2.position p2.velocity in
  distance3 p1' p2' < 2.0

let manhattan_distance a b x y = abs (a - x) + abs (b - y)

let euclid a b =
  if a <= 0 || b <= 0 then invalid_arg "euclid"
  else
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    gcd a b

let rec ackermann m n =
  if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))

let approximate_pi n =
  if n <= 10000 then failwith "approximate_pi"
  else
    let rec f k approximate =
      if k < 1 then 2.0
      else
        let x = 2.0 *. float_of_int k in
        let y = x *. x in
        f (k - 1) approximate *. y /. (y -. 1.)
    in
    f n 2.0

(* Implement a function digits : int -> int -> int*int*int which receives an integer and a digit ,
   and returns the number of times the digit  appears in ,
   the sum of the digits of and finally the number of digits of .
   For example, digits 1073741823 3 would return the tuple (2,36,10).
*)
let digits n i =
  let rec f k i acc =
    if k = 0 then acc
    else
      let k' = k / 10 in
      let m = k mod 10 in
      let a, b, c = acc in
      if m = i then f k' i (a + 1, b + m, c + 1) else f k' i (a, b + m, c + 1)
  in
  f n i (0, 0, 0)

(* let rec hhq r s n =
   if n < 1 then 0
   else if n = 1 then 1
   else
     let a = n - hhq r s (n - r) in
     let b = n - hhq r s (n - s) in
     hhq r s a + hhq r s b *)

let hfm n =
  let rec hfm' k acc =
    if k < n + 1 then (
      let fk1 = fst acc.(k - 1) in
      let mk1 = snd acc.(k - 1) in
      acc.(k) <- (k - snd acc.(fk1), k - fst acc.(mk1));
      hfm' (k + 1) acc)
  in
  let arr = Array.make (n + 1) (0, 0) in
  arr.(0) <- (1, 0);
  hfm' 1 arr;
  (* Array.iter (fun (f, m) -> Printf.printf "%d %d\n" f m) arr; *)
  arr.(n)
