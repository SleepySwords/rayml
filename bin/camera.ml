open Vec3
open World

type camera = {
  aspect_ratio : float;
  image_width : int;
  image_height : int;
  centre : vec3;
  pixel00_loc : vec3;
  pixel_delta_u : vec3;
  pixel_delta_v : vec3;
  samples_per_pixel : int;
  max_depth : int;
}

let aspect_ratio = 16.0 /. 9.0

let output_metadata { image_width; image_height } _ =
  Printf.printf "P3\n%d %d\n255\n" image_width image_height

let output_pixel r g b _ = Printf.printf "%d %d %d\n" r g b

let rec ray_colour ray depth world =
  let open Vec3 in
  if depth <= 0 then Vec3.make 0. 0. 0.
  else
    let rc =
      GenericHittableList.hit world ray
        Interval.{ min_i = 0.001; max_i = infinity }
    in
    match rc with
    | Some { mat; point; normal; front_face } -> (
        match mat ray point normal front_face with
        | Some (attenuation, scattered) ->
            Vec3.multiply_vec attenuation
              (ray_colour scattered (depth - 1) world)
        | _ -> Vec3.zero)
    | None ->
        let { y } = Vec3.unit_vector ray.direction in
        let a = 0.5 *. (y +. 1.0) in
        ((1.0 -. a) *.. Vec3.make 1.0 1.0 1.0) +.. (a *.. Vec3.make 0.5 0.7 1.0)

let degrees_to_radians deg = deg *. Float.pi /. 180.

let make ?(max_depth = 10) ?(vfov = 90.) ?(samples_per_pixel = 10)
    ?(look_from = Vec3.make 0. 0. 0.) ?(look_at = Vec3.make 0. 0. (-1.))
    ?(vup = Vec3.make 0. 1. 0.) image_width aspect_ratio =
  let camera_centre = Vec3.make 0.0 0.0 0.0 in

  let image_height = int_of_float (float_of_int image_width /. aspect_ratio) in

  let focal_length = 1.0 in
  let theta = degrees_to_radians vfov in
  let h = tan (theta /. 2.) in
  let viewport_height = 2. *. h *. focal_length in
  let viewport_width =
    viewport_height *. (float_of_int image_width /. float_of_int image_height)
  in

  let viewpoint_u = Vec3.make viewport_width 0.0 0.0 in
  let viewpoint_v = Vec3.make 0.0 (-.viewport_height) 0.0 in

  let pixel_delta_u = viewpoint_u /.. float_of_int image_width in
  let pixel_delta_v = viewpoint_v /.. float_of_int image_height in

  let viewport_upper_left =
    camera_centre
    -.. Vec3.make 0.0 0.0 focal_length
    -.. (viewpoint_u /.. 2.) -.. (viewpoint_v /.. 2.)
  in
  let pixel00_loc =
    viewport_upper_left +.. (0.5 *.. (pixel_delta_u +.. pixel_delta_v))
  in

  {
    aspect_ratio;
    image_width;
    image_height;
    centre = Vec3.make 0.0 0.0 0.0;
    pixel00_loc;
    pixel_delta_u;
    pixel_delta_v;
    samples_per_pixel;
    max_depth;
  }

let get_ray { pixel00_loc; pixel_delta_u; pixel_delta_v; centre } i j =
  let offset = Vec3.sample_square () in
  let pixel_centre =
    pixel00_loc
    +.. ((float_of_int i +. offset.x) *.. pixel_delta_u)
    +.. ((float_of_int j +. offset.y) *.. pixel_delta_v)
  in

  let ray_direction = pixel_centre -.. centre in
  Ray.{ direction = ray_direction; origin = centre }

let render world
    ({
       pixel00_loc;
       image_width;
       image_height;
       pixel_delta_u;
       pixel_delta_v;
       centre;
       samples_per_pixel;
       max_depth;
     } as camera) =
  output_metadata camera ();
  for j = 0 to image_height - 1 do
    Printf.eprintf "\rScanlines remaining: %d %!" (image_height - j);
    for i = 0 to image_width - 1 do
      let pixel_colour = ref (Vec3.make 0.0 0.0 0.0) in
      for sample = 0 to samples_per_pixel - 1 do
        let ray = get_ray camera i j in
        pixel_colour := !pixel_colour +.. ray_colour ray max_depth world
      done;
      Vec3.output_colour stdout
        (!pixel_colour /.. float_of_int samples_per_pixel)
        ()
    done
  done;
  Printf.eprintf "\rDone                                    \n"
