let n = ref 6

let shape () =
  let open Painter in
  let gen shape =
    let lst = Shape.dest_polygon shape in
    let shapes = List.map (fun v -> Shape.enlarge v 0.5 shape) lst in
    fun n ->
      try List.nth shapes n
      with _ -> raise Shape.Iter_stop in
  Shape.Recursive
    (gen,
     Shape.regular_polygon (Vector2D.mk 0.5 0.5) 3 0.5,
     !n)
