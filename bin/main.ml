open Sphere
open Material
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )
let material_ground = lambertian_scatter ~albedo:(Vec3.make 0.8 0.8 0.0)
let material_centre = lambertian_scatter ~albedo:(Vec3.make 0. 0.2 0.5)
let material_left = dialetic_scatter ~refraction_index:1.5
let material_bubble = dialetic_scatter ~refraction_index:(1. /. 1.5)
let material_right = metal_scatter ~albedo:(Vec3.make 0.8 0.6 0.2) ~fuzz:1.0
let r = cos (Float.pi /. 4.)

let world =
  [
    mkSphereGen
      { centre = Vec3.make (-.r) 0. (-1.); radius = r; mat = material_ground };
    mkSphereGen
      { centre = Vec3.make r 0. (-1.); radius = r; mat = material_centre };
  ]

let aspect_ratio = 16.0 /. 9.0
let image_width = 800

let camera =
  Camera.make ~samples_per_pixel:100 ~vfov:90. image_width aspect_ratio

let () =
  Camera.render world camera;
  Random.self_init ();
  prerr_float (Random.float 1.0)
