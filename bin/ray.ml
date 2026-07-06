open Vec3

type ray = { origin : Vec3.vec3; direction : Vec3.vec3 }

let at { origin; direction } t = origin +.. (t *.. direction)

let hit_sphere (centre : vec3) radius ray =
  let oc = centre -.. ray.origin in
  let a = Vec3.length_sqrd ray.direction in
  let h = dotproduct ray.direction oc in
  let c = length_sqrd oc -. (radius *. radius) in
  let disc = (h *. h) -. (a *. c) in

  if disc < 0.0 then -1. else (h -. sqrt disc) /. a
