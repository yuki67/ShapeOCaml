let n = ref 128
let k = ref 8

let shape () = Gredients.circloid
    (fun i j -> (i + j) mod (!n / !k) = 0) !n
