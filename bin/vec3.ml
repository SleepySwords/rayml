(* might convert this into an array rather than a record*)
type vec3 = { x : float; y : float; z : float }

let zero = { x = 0.0; y = 0.0; z = 0.0 }

let[@inline always] ( +.. ) ({ x = x1; y = y1; z = z1 } : vec3)
    ({ x = x2; y = y2; z = z2 } : vec3) =
  { x = x1 +. x2; y = y1 +. y2; z = z1 +. z2 }

let[@inline always] neg ({ x; y; z } : vec3) = { x = -.x; y = -.y; z = -.z }
let[@inline always] ( -.. ) v1 v2 = v1 +.. neg v2
let[@inline always] ( *.. ) t ({ x; y; z } : vec3) = { x = x *. t; y = y *. t; z = z *. t }
let[@inline always] ( /.. ) v t = 1. /. t *.. v
let[@inline always] length_sqrd v = (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)
let[@inline always] length v = sqrt @@ length_sqrd v

let[@inline always] multiply_vec v1 v2 =
  { x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z }

let[@inline always] make x y z = { x; y; z }
let[@inline always] unit_vector v = v /.. length v
let[@inline always] dotproduct v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)

let[@inline always] crossproduct v1 v2 =
  {
    x = (v1.y *. v2.z) -. (v1.z *. v2.y);
    y = -.((v1.x *. v2.z) -. (v1.z *. v2.x));
    z = (v1.x *. v2.y) -. (v1.y *. v2.x);
  }

let[@inline always] output stream v () = Printf.fprintf stream "%f %f %f" v.x v.y v.z
let pixel_interval = Interval.{ min_i = 0.0; max_i = 0.999 }
let[@inline always] linear_to_gamma component = sqrt @@ max component 0.

let output_colour stream v () =
  [ v.x; v.y; v.z ] |> List.map linear_to_gamma
  |> List.map (Interval.clamp pixel_interval)
  |> List.map (fun x -> x *. 255.999)
  |> List.map int_of_float |> List.map string_of_int |> String.concat " "
  |> Printf.fprintf stream "%s\n"

let sample_square () =
  make (Random.float 1.0 -. 0.5) (Random.float 1.0 -. 0.5) 0.0

let random_vec () =
  make (Random.float 1.0) (Random.float 1.0) (Random.float 1.0)

let random_float_range min max = (Random.float (max -. min)) +. min

let random_vec_range min max =
  make
    (random_float_range min max)
    (random_float_range min max)
    (random_float_range min max)

let random_unit_vec () =
  let p = ref (random_vec_range (-1.) 1.) in
  while 1e-160 >= length_sqrd !p || length_sqrd !p > 1. do
    p := random_vec_range (-1.) 1.
  done;
  !p

let random_in_unit_disc () =
  let p =
    ref @@ make (random_float_range (-1.) 1.) (random_float_range (-1.) 1.) 0.
  in
  while length_sqrd !p >= 1. do
    p := make (random_float_range (-1.) 1.) (random_float_range (-1.) 1.) 0.
  done;
  !p

let random_on_hemisphere normal =
  let on_unit_sphere = random_unit_vec () in
  if dotproduct on_unit_sphere normal > 0.0 then on_unit_sphere
  else neg on_unit_sphere

let small_value = 1e-8

let near_zero v =
  abs_float v.x < small_value
  && abs_float v.y < small_value
  && abs_float v.z < small_value

let reflect v n = v -.. (2. *. dotproduct v n *.. n)

let refract uv n etai_over_etat =
  let cos_theta = min (dotproduct (neg uv) n) 1. in
  let r_out_perp = etai_over_etat *.. (uv +.. (cos_theta *.. n)) in
  let r_out_parallel =
    -.sqrt (abs_float (1.0 -. length_sqrd r_out_perp)) *.. n
  in
  r_out_perp +.. r_out_parallel
