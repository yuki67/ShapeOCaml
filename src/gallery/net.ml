open Painter.Vector2D

let d = ref 0.1
let n = ref 7

let shape () =
  Gredients.square_subtransform (fun v -> !d *.. v) !n
