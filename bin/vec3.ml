type vec3 = {
    x : float;
    y : float;
    z : float;
}

let zero = { x = 0.0; y = 0.0; z = 0.0 }

let ( +.. ) ({x = x1; y = y1 ;z = z1}: vec3) ({x = x2; y = y2; z = z2}: vec3) = { x = x1 +. x2; y = y1 +. y2; z = z1 +. z2;}

let neg ({x; y; z}: vec3) = { x = -.x; y = -.y; z = -.z;}

let ( -.. ) v1 v2 = v1 +.. (neg v2)

let ( *.. ) t ({x; y; z}: vec3) = { x = x *. t; y = y *. t; z = z *. t;}

let ( /.. ) v t =  (1./.t) *.. v

let length_sqrd v = v.x *. v.x +. v.y *. v.y +. v.z *. v.z

let length v = sqrt @@ length_sqrd v


let make x y z = {x; y; z}

let unit_vector v = v /.. (length v)

let dotproduct v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let crossproduct v1 v2 = {
    x = v1.y *. v2.z -. v1.z *. v2.y;
    y = v1.x *. v2.z -. v1.z *. v2.x;
    z = v1.x *. v2.y -. v1.y *. v2.x;
}

let output stream v () = Printf.fprintf stream "%f %f %f" v.x v.y v.z

let output_colour stream v () = [v.x; v.y; v.z;]
    |> List.map (fun x -> x *. 255.999 )
    |> List.map int_of_float
    |> List.map string_of_int
    |> String.concat " "
    |> Printf.fprintf stream "%s\n"
