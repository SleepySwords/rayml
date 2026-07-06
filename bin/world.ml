open Sphere

type generic_hittable = GenSphere of Sphere.t

module GenericHittable : Hit.Hittable with type t = generic_hittable = struct
  type t = generic_hittable

  let hit h r interval =
    match h with GenSphere sph -> Sphere.hit sph r interval
end

let mkSphereGen sph = GenSphere sph

module GenericHittableList : Hit.Hittable with type t = GenericHittable.t list =
struct
  type t = GenericHittable.t list

  let hit list ray Interval.{ min_i = tmin; max_i = tmax } =
    List.fold_left
      (fun (hit, closest) gh ->
        match
          GenericHittable.hit gh ray Interval.{ min_i = tmin; max_i = closest }
        with
        | Some rc when closest > rc.t -> (Some rc, rc.t)
        | _ -> (hit, closest))
      (None, tmax) list
    |> fst
end

module type StorableHittable = sig
  type t

  val value : t
  val md : (module Hit.Hittable with type t = t)
end

let hit_module (module S : StorableHittable) =
  let (module K) = S.md in
  K.hit S.value

let mkSphereMod sph : (module StorableHittable) =
  (module struct
    type t = sphere

    let value = sph
    let md = (module Sphere : Hit.Hittable with type t = t)
  end)

module ModuleHittableList :
  Hit.Hittable with type t = (module StorableHittable) list = struct
  type t = (module StorableHittable) list

  let hit lst ray Interval.{ min_i = tmin; max_i = tmax } =
    List.fold_left
      (fun (hit, closest) md ->
        match hit_module md ray Interval.{ min_i = tmin; max_i = closest } with
        | Some rc when closest > rc.t -> (Some rc, rc.t)
        | _ -> (hit, closest))
      (None, tmax) lst
    |> fst
end
