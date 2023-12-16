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
