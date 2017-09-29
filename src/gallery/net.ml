open Painter.Vector2D

let n = ref 7

let shape () =
  Gredients.square_subtransform (fun d -> 0.1 *.. d) !n
