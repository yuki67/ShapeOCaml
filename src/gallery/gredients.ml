open MyExt.ListExt
open Painter.Shape
open Painter.Vector2D

let square_subtransform f n =
  let gen shape =
    let p0, p2, p8, p6 =
      match dest_polygon shape with
      | [a;b;c;d] -> a, b, c, d
      | _ -> failwith "squre_subtransform: shape is not a square!" in
    let p1, p3, p5, p7 =
      0.5 *.. (p0 +.. p2),
      0.5 *.. (p0 +.. p6),
      0.5 *.. (p2 +.. p8),
      0.5 *.. (p6 +.. p8) in
    let delta = p8 -.. p0 in
    let p4 = 0.25 *.. (p0 +.. p2 +.. p6+.. p8) +.. (f delta) in
    function
    | 0 -> Polygon ([p0;p1;p4;p3])
    | 1 -> Polygon ([p1;p2;p5;p4])
    | 2 -> Polygon ([p4;p5;p8;p7])
    | 3 -> Polygon ([p3;p4;p7;p6])
    | _ -> raise Iter_stop in
  Recursive (gen, Polygon [mk 0.0 0.0; mk 0.0 1.0; mk 1.0 1.0; mk 1.0 0.0], n)

let circloid f n =
  let points = regular_polygon_array (mk 0.5 0.5) n 0.5 in
  all_pairs (fun x y -> x, y) (range n) (range n)
  |> filter (fun (x, y) -> f x y)
  |> map (fun (x, y) -> Line (points.(x), points.(y)))
  |> fun x -> ShapeList x

let shrink_to_points ?(r=0.5) poly n =
  let lst = dest_polygon poly in
  enlarge (nth lst n) r poly
