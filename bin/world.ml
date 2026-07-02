open Sphere

type generic_hittable =
    | GenSphere of Sphere.t

module GenericHittable : Hit.Hittable with type t = generic_hittable = struct
    type t = generic_hittable

    let hit h r tmin tmax = match h with
    | GenSphere sph -> Sphere.hit sph r tmin tmax
end

let mkSphereGen sph = GenSphere sph

module GenericHittableList : Hit.Hittable with type t = GenericHittable.t list = struct
    type t = GenericHittable.t list

    let hit list ray tmin tmax =
        let hit = ref None in
        let closest = ref tmax in
        List.iter (fun gh  ->
            match (GenericHittable.hit gh ray tmin !closest) with
            | Some rc -> if !closest > rc.t then (
                hit := Some rc;
                closest := rc.t;
            )
            | None -> ()
        ) list;
        !hit
end

module type StorableHittable = sig
    type t

    val value : t

    val md : (module Hit.Hittable with type t = t)
end

let hit_module (module S : StorableHittable) = let (module K) = S.md in K.hit S.value

let mkSphereMod sph : (module StorableHittable) = (module struct
    type t = sphere

    let value = sph
    let md = (module Sphere : Hit.Hittable with type t = t)
end)

module ModuleHittableList : Hit.Hittable with type t = (module StorableHittable) list = struct
    type t = (module StorableHittable) list

    let hit list ray tmin tmax =
        let hit = ref None in
        let closest = ref tmax in
        List.iter (fun md  ->
            match (hit_module md ray tmin !closest) with
            | Some rc -> if !closest > rc.t then (
                hit := Some rc;
                closest := rc.t;
            )
            | None -> ()
        ) list;
        !hit
end
