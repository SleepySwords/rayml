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

let world =
  [
    mkSphereGen
      {
        centre = Vec3.make 0.0 (-100.5) (-1.0);
        radius = 100.0;
        mat = material_ground;
      };
    mkSphereGen
      { centre = Vec3.make 0.0 0.0 (-1.2); radius = 0.5; mat = material_centre };
    mkSphereGen
      {
        centre = Vec3.make (-1.0) 0.0 (-1.0);
        radius = 0.5;
        mat = material_left;
      };
    mkSphereGen
      {
        centre = Vec3.make (-1.0) 0.0 (-1.0);
        radius = 0.4;
        mat = material_bubble;
      };
    mkSphereGen
      { centre = Vec3.make 1.0 0.0 (-1.0); radius = 0.5; mat = material_right };
  ]

let aspect_ratio = 16.0 /. 9.0
let image_width = 400

let camera =
  Camera.make ~samples_per_pixel:100 ~vfov:20.
    ~look_from:(Vec3.make (-2.) 2. 1.) ~look_at:(Vec3.make 0. 0. (-1.))
    ~defocus_angle:10. ~focus_dist:3.4 image_width aspect_ratio

let () =
  Random.self_init ();
  Camera.render world camera
