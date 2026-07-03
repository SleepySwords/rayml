open Vec3

(* Input ray, point of hit, normal, produced attenuation and ray*)
type material_scatter = Ray.ray -> Vec3.vec3 -> Vec3.vec3 -> (Vec3.vec3 * Ray.ray) option

let lambertian_scatter ~albedo ray point normal = 
    let scatter_direction = normal +.. Vec3.random_unit_vec () in
    Some (albedo, Ray.{origin = point; direction = if Vec3.near_zero scatter_direction then normal else scatter_direction})

let metal_scatter ~albedo ~fuzz ray point normal = 
    let reflected = Vec3.reflect Ray.(ray.direction) normal in
    let fuzzed = unit_vector reflected +.. (fuzz *.. random_unit_vec ()) in
    let scattered = Ray.{origin = point; direction = fuzzed} in
    if dotproduct scattered.direction normal > 0. then
        Some (albedo, scattered)
    else 
        None
