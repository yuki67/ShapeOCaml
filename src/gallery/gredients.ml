let square_subtransform f n =
  let open Painter in
  let open Painter.Vector2D in
  let gen shape =
    let p0, p2, p8, p6 =
      match Shape.dest_polygon shape with
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
    | 0 -> Shape.Polygon ([p0;p1;p4;p3])
    | 1 -> Shape.Polygon ([p1;p2;p5;p4])
    | 2 -> Shape.Polygon ([p4;p5;p8;p7])
    | 3 -> Shape.Polygon ([p3;p4;p7;p6])
    | _ -> raise Shape.Iter_stop in
  Shape.Recursive
    (gen,
     Shape.Polygon
       [mk 0.0 0.0;
        mk 0.0 1.0;
        mk 1.0 1.0;
        mk 1.0 0.0],
     n)

let circloid f n =
  let open Painter in
  let open MyExt in
  let points = Shape.regular_polygon_array (Vector2D.mk 0.5 0.5) n 0.5 in
  ListExt.all_pairs (fun x y -> x, y) (ListExt.range n) (ListExt.range n)
  |> List.filter (fun (x, y) -> f x y)
  |> List.map (fun (x, y) -> Shape.Line (points.(x), points.(y)))
  |> fun x -> Shape.ShapeList x
