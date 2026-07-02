open Sphere
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )

let world = [
    mkSphereMod {centre = Vec3.make 0.0 0.0 (-.1.0); radius = 0.5};
    mkSphereMod {centre = Vec3.make 0.0 (-.100.5) (-.1.0); radius = 100.0};
]


let aspect_ratio = 16.0 /. 9.0
let image_width = 800
let camera = Camera.make image_width aspect_ratio 100

let () = Camera.render world camera; Random.self_init (); prerr_float (Random.float 1.0)
