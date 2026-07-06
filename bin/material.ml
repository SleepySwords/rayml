open Vec3

(* Input ray, point of hit, normal, produced attenuation and ray*)
type material_scatter =
  Ray.ray -> Vec3.vec3 -> Vec3.vec3 -> bool -> (Vec3.vec3 * Ray.ray) option

let lambertian_scatter ~albedo ray point normal _ =
  let scatter_direction = normal +.. Vec3.random_unit_vec () in
  Some
    ( albedo,
      Ray.
        {
          origin = point;
          direction =
            (if Vec3.near_zero scatter_direction then normal
             else scatter_direction);
        } )

let metal_scatter ~albedo ~fuzz ray point normal _ =
  let reflected = Vec3.reflect Ray.(ray.direction) normal in
  let fuzzed = unit_vector reflected +.. (fuzz *.. random_unit_vec ()) in
  let scattered = Ray.{ origin = point; direction = fuzzed } in
  if dotproduct scattered.direction normal > 0. then Some (albedo, scattered)
  else None

let reflectance cosine refraction_index =
  let r0 = (1. -. refraction_index) /. (1. +. refraction_index) in
  let r0 = r0 *. r0 in
  r0 +. ((1. -. r0) *. ((1. -. cosine) ** 5.))

let dialetic_scatter ~refraction_index ray point normal front_face =
  let ri = if front_face then 1. /. refraction_index else refraction_index in
  let unit_direction = unit_vector Ray.(ray.direction) in
  let cos_theta = min (dotproduct (neg unit_direction) normal) 1.0 in
  let sin_theta = sqrt (1. -. (cos_theta *. cos_theta)) in
  let cannot_refract = ri *. sin_theta > 1.0 in
  let direction =
    if cannot_refract || reflectance cos_theta ri > random_float_range 0. 1.
    then Vec3.reflect unit_direction normal
    else Vec3.refract unit_direction normal ri
  in
  Some (Vec3.make 1. 1. 1., Ray.{ origin = point; direction })
