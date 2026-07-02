open Sphere

type generic_hittable =
    | GenSphere of Sphere.t

module GenericHittable : Hit.Hittable with type t = generic_hittable = struct
    type t = generic_hittable

    let hit h r tmin tmax = match h with
    | GenSphere sph -> Sphere.hit sph r tmin tmax
end

let mkSphere sph = GenSphere sph

module HittableList : Hit.Hittable with type t = GenericHittable.t list = struct
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
