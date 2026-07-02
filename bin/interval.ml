type interval = {
    min_i : float;
    max_i : float;
}

let size {min_i; max_i; } = max_i -. min_i

let contains {min_i; max_i;} x = min_i <= x && x <= max_i

let surrounds {min_i; max_i;} x = min_i < x && x < max_i

let empty = {min_i = infinity; max_i = neg_infinity}

let universe = {min_i = neg_infinity; max_i = infinity}

let clamp {min_i; max_i} x = min max_i x |> max min_i
