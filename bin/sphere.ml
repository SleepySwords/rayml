open Hit
open Vec3
open Ray

type sphere = {
  centre : Vec3.vec3;
  radius : float;
  mat : Material.material_scatter;
}

module Sphere : Hittable with type t = sphere = struct
  type t = sphere

  let hit { centre; radius; mat } ray Interval.{ min_i = tmin; max_i = tmax } =
    let oc = centre -.. ray.origin in
    let a = Vec3.length_sqrd ray.direction in
    let h = dotproduct ray.direction oc in
    let c = length_sqrd oc -. (radius *. radius) in
    let disc = (h *. h) -. (a *. c) in
    let create_hit_record root = 
        let p = Ray.at ray root in
        let outward_normal = (p -.. centre) /.. radius in
        let front_face, normal = face_normal ray outward_normal in
        Some { t = root; point = p; normal; front_face; mat } in

    if disc < 0.0 then None
    else
      let plus_root = (h +. sqrt disc) /. a in
      let minus_root = (h -. sqrt disc) /. a in
      if tmin <= minus_root && minus_root <= tmax then create_hit_record minus_root
      else if tmin <= plus_root  && plus_root <= tmax then create_hit_record plus_root
      else None
end
