let image_height = 256
let image_width = 256

let output_metadata _ = Printf.printf "P3\n%d %d\n255\n" image_width image_height

let output_pixel r g b _ = Printf.printf "%d %d %d\n" r g b

let output_sample _ = output_metadata (); for j = 0 to (image_height - 1) do
    Printf.eprintf "\rScanlines remaining: %d " (image_height - j);
    for i = 0 to (image_width - 1) do
        let r = float_of_int i /. (float_of_int image_width  -. 1.) in
        let g = float_of_int j /. (float_of_int image_height  -. 1.) in
        let colour = Vec3.make r g 0.0 in
        Vec3.output_colour stdout colour ()
    done
done;
Printf.eprintf "\rDone                                    \n"

let () = output_sample ()
