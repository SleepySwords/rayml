open Sphere
open Material
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )

let material_ground = lambertian_scatter ~albedo:(Vec3.make 0.8 0.8 0.0)
let material_centre = lambertian_scatter ~albedo:(Vec3.make 0. 0.2 0.5)
let material_left = metal_scatter ~albedo:(Vec3.make 0.8 0.8 0.8) ~fuzz:0.3
let material_right = metal_scatter ~albedo:(Vec3.make 0.8 0.6 0.2) ~fuzz:1.0

let world = [
    mkSphereMod {centre = Vec3.make 0.0 (-.100.5) (-.1.0); radius = 100.0; mat = material_ground};
    mkSphereMod {centre = Vec3.make 0.0 0.0 (-.1.2); radius = 0.5; mat = material_centre};
    mkSphereMod {centre = Vec3.make (-.1.0) 0.0 (-.1.0); radius = 0.5; mat = material_left};
    mkSphereMod {centre = Vec3.make 1.0 0.0 (-.1.0); radius = 0.5; mat = material_right};
]


let aspect_ratio = 16.0 /. 9.0
let image_width = 1920
let camera = Camera.make image_width aspect_ratio 100

let () = Camera.render world camera; Random.self_init (); prerr_float (Random.float 1.0)
