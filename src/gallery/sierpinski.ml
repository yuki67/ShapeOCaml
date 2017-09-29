open MyExt.ListExt
open Painter.Shape
open Painter.Vector2D

let n = ref 6

let shape () =
  let gen shape =
    let lst = dest_polygon shape in
    let shapes = map (fun v -> enlarge v 0.5 shape) lst in
    fun n ->
      try nth shapes n
      with _ -> raise Iter_stop
  in
  Recursive (gen, regular_polygon (mk 0.5 0.5) 3 0.5, !n)
