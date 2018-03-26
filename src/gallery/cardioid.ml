let n = ref 256
let k = ref 2

let shape () =
  Gredients.circloid
    (fun i j ->  (!k * i) mod !n = j)
    !n
