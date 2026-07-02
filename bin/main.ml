open Sphere
open World

let ( +.. ) = Vec3.( +.. )
let ( -.. ) = Vec3.( -.. )
let ( *.. ) = Vec3.( *.. )
let ( /.. ) = Vec3.( /.. )

let world = [
    mkSphere {centre = Vec3.make 0.0 0.0 (-.1.0); radius = 0.5};
    mkSphere {centre = Vec3.make 0.0 (-.100.5) (-.1.0); radius = 100.0};
]

let ray_colour (ray: Ray.ray) =
    let open Vec3 in
    let rc = HittableList.hit world ray 0. infinity in
    match rc with
    | Some r -> 0.5 *.. (r.normal +.. Vec3.make 1.0 1.0 1.0)
    | None -> (
        let { y } = Vec3.unit_vector ray.direction in
        let a = 0.5 *. (y +. 1.0) in
        ((1.0 -. a) *.. Vec3.make 1.0 1.0 1.0) +.. a *.. Vec3.make 0.5 0.7 1.0
    )


let aspect_ratio = 16.0 /. 9.0

let image_width = 1920
let image_height = int_of_float (float_of_int image_width /. aspect_ratio)

let focal_length = 1.0
let viewport_height = 2.0
let viewport_width = viewport_height *. (float_of_int image_width /. float_of_int image_height)

let output_metadata _ = Printf.printf "P3\n%d %d\n255\n" image_width image_height
let output_pixel r g b _ = Printf.printf "%d %d %d\n" r g b

let output_sample _ =
    let camera_centre = Vec3.make 0.0 0.0 0.0 in

    let viewpoint_u = Vec3.make (viewport_width) 0.0 0.0 in
    let viewpoint_v = Vec3.make 0.0 (-.viewport_height) 0.0 in

    let pixel_delta_u =  viewpoint_u /.. (float_of_int image_width) in
    let pixel_delta_v = viewpoint_v /.. (float_of_int image_height) in

    let viewport_upper_left = camera_centre -.. (Vec3.make 0.0 0.0 focal_length) -.. (viewpoint_u /.. 2.) -.. (viewpoint_v /.. 2.) in
    let pixel00_loc = viewport_upper_left +.. 0.5 *.. (pixel_delta_u +.. pixel_delta_v) in

    output_metadata ();
    for j = 0 to (image_height - 1) do
        Printf.eprintf "\rScanlines remaining: %d %!" (image_height - j);
        for i = 0 to (image_width - 1) do
            let pixel_centre = pixel00_loc +.. (float_of_int i *.. pixel_delta_u) +.. (float_of_int j *.. pixel_delta_v) in

            let ray_direction = pixel_centre -.. camera_centre in
            let ray: Ray.ray = {direction = ray_direction; origin = camera_centre} in
            let pixel_colour = ray_colour ray in

            Vec3.output_colour stdout pixel_colour ()
        done
    done;
    Printf.eprintf "\rDone                                    \n"

let () = output_sample ()
