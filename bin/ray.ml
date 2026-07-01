open Vec3

type ray = {
    origin : Vec3.vec3;
    direction : Vec3.vec3;
}

let at {origin; direction} t = origin +.. (t *.. direction)

let hit_sphere (centre: vec3) radius ray =
    let oc = centre -.. ray.origin in
    let a = dotproduct ray.direction ray.direction in
    let b = -2.0 *. dotproduct (ray.direction) oc in
    let c = dotproduct oc oc -. radius *. radius in
    let disc = b *. b -. 4. *. a *. c in

    disc >= 0.
