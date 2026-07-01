open Vec3

type ray = {
    origin : Vec3.vec3;
    direction : Vec3.vec3;
}

let at {origin; direction} t = origin +| (t *| direction)
