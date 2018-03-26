let d = ref 0.1
let n = ref 7

let shape () =
  let open ShapeOCaml.Vector2D in
  Gredients.square_subtransform (fun v -> !d *.. v) !n
