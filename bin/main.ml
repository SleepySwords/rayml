open Sphere
open Material
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )
let material_ground = lambertian_scatter ~albedo:(Vec3.make 0.5 0.5 0.5)
let material1 = dialetic_scatter ~refraction_index:1.5
let material2 = lambertian_scatter ~albedo:(Vec3.make 0.4 0.2 0.1)
let material3 = metal_scatter ~albedo:(Vec3.make 0.7 0.6 0.5) ~fuzz:0.
let materials = [| material_ground; material1; material2; material3 |]

let world =
  [
    mkSphereGen
      { centre = Vec3.make 0.0 (-1000.) 0.0; radius = 1000.0; mat = 0 };
    mkSphereGen { centre = Vec3.make 0.0 1. 0.0; radius = 1.0; mat = 1 };
    mkSphereGen { centre = Vec3.make (-4.) 1. 0.; radius = 1.0; mat = 2 };
    mkSphereGen { centre = Vec3.make 4. 1. 0.; radius = 1.0; mat = 3 };
  ]

let (customSpheres, customMaterials) =
  List.init (22 * 22) (fun x -> x)
  |> List.fold_left
       (fun (spheres, materials) i ->
         let a = (i / 22) - 11 in
         let b = (i mod 22) - 11 in
         let choose_chance = Vec3.random_float_range 0. 1. in
         let mat =
           match choose_chance with
           | x when x < 0.8 ->
               lambertian_scatter
                 ~albedo:
                   (Vec3.multiply_vec
                      (Vec3.random_vec_range 0. 1.)
                      (Vec3.random_vec_range 0. 1.))
           | x when x < 0.95 ->
               metal_scatter
                 ~albedo:(Vec3.random_vec_range 0.5 1.)
                 ~fuzz:(Vec3.random_float_range 0. 0.5)
           | otherwise -> dialetic_scatter ~refraction_index:1.5
         in
         ( mkSphereGen
             {
               centre =
                 Vec3.make
                   (float_of_int a +. Vec3.random_float_range (-0.9) 0.9)
                   0.2
                   (float_of_int b +. Vec3.random_float_range (-0.9) 0.9);
               radius = 0.2;
               mat = List.length materials;
             }
           :: spheres,
           mat :: materials ))
       ([], [])

let world = world @ customSpheres
let materials = Array.append materials (Array.of_list customMaterials)

let aspect_ratio = 16.0 /. 9.0
let image_width = 100

let camera =
  Camera.make ~samples_per_pixel:500 ~vfov:20. ~max_depth:50
    ~look_from:(Vec3.make 13. 2. 3.) ~look_at:(Vec3.make 0. 0. 0.)
    ~defocus_angle:0.6 ~focus_dist:10. image_width aspect_ratio

let () =
  Random.self_init ();
  prerr_endline "";
  let start_time = Sys.time () in
  Camera.render (world, materials) camera;
  let stop_time = Sys.time () in
  Printf.eprintf "Computed %d rays in %fs, (%f million rays/s)\n"
    Camera.(total_rays ())
    (stop_time -. start_time)
    (float_of_int Camera.(total_rays ()) /. (stop_time -. start_time) /. 1e6)
