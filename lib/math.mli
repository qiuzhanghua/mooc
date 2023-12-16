val distance2 : float * float -> float * float -> float
val area : float -> float
val sin2x : float -> float

type point = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

val move : point -> dpoint -> point
val will_collide_soon : physical_object -> physical_object -> bool
val manhattan_distance : int -> int -> int -> int -> int
val euclid : int -> int -> int
val ackermann : int -> int -> int
val approximate_pi : int -> float
