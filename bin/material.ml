open Vec3

(* Input ray, point of hit, normal, produced attenuation and ray*)
type material_scatter = Ray.ray -> Vec3.vec3 -> Vec3.vec3 -> (Vec3.vec3 * Ray.ray) option

let lambertian_scatter ~colour ray point normal = 
    let scatter_direction = normal +.. Vec3.random_unit_vec () in
    Some (colour, Ray.{origin = point; direction = scatter_direction})
