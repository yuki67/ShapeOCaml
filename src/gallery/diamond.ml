open Gredients

let n = ref 32

let shape () =
  Gredients.circloid (fun _ _ -> true) !n
