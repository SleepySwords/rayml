open Sphere
open Material
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )

let world = [
    mkSphereMod {centre = Vec3.make 0.0 0.0 (-.1.0); radius = 0.5; mat = lambertian_scatter ~colour:(Vec3.make 0.8 0.8 0.0)};
    mkSphereMod {centre = Vec3.make 0.0 (-.100.5) (-.1.0); radius = 100.0; mat = lambertian_scatter ~colour:(Vec3.make 0.8 0.8 0.0)};
]


let aspect_ratio = 16.0 /. 9.0
let image_width = 1920
let camera = Camera.make image_width aspect_ratio 100

let () = Camera.render world camera; Random.self_init (); prerr_float (Random.float 1.0)
