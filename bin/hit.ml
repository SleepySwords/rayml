type hit_record = {
  point : Vec3.vec3;
  normal : Vec3.vec3;
  t : float;
  mat : int;
  front_face : bool;
}

let face_normal ray outward_normal =
  let front_face = Vec3.dotproduct Ray.(ray.direction) outward_normal < 0. in
  (front_face, if front_face then outward_normal else Vec3.neg outward_normal)

(* It might be better to replace this with a function, we can do partial application in that scenario*)
module type Hittable = sig
  type t

  val hit : t -> Ray.ray -> Interval.interval -> hit_record option
end
