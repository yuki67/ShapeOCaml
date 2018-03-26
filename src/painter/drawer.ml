(* no exhoustiveness check. *)
let drawer_alist =
  ref [ (Shape.POINT, None);
        (Shape.LINE, None);
        (Shape.POLYGON, None);
        (Shape.CIRCLE, None)]

let add_drawer prim_type f =
  drawer_alist := (prim_type, Some f) :: !drawer_alist

let rec draw_point shape =
  match List.assoc Shape.POINT !drawer_alist with
  | Some drawer -> drawer shape
  | None -> failwith "No way to draw a shape!"

and draw_line shape =
  match List.assoc Shape.LINE !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
    let p, q = Shape.dest_line shape in
    let ts = MyExt.ListExt.linspace 0.0 1.0 50 in
    let points = List.map (fun r -> Shape.Point (Vector2D.lerp p q r)) ts in
    List.iter draw points

and draw_polygon shape =
  match List.assoc Shape.POLYGON !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
    let lst = Shape.dest_polygon shape in
    let lines = MyExt.ListExt.loop_pairs (fun p q -> Shape.Line (p, q)) lst in
    List.iter draw lines

and draw_recursive shape =
  let gen, init, n = Shape.dest_recursive shape in
  let rec loop shape n =
    if n = 0 then draw shape
    else
      let gen' = gen shape in
      let rec loop2 i =
        loop (gen' i) (n - 1);
        loop2 (i + 1) in
      try loop2 0 with Shape.Iter_stop -> () in
  loop init n

and draw_circle shape =
  match List.assoc Shape.CIRCLE !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
    let p, r = Shape.dest_circle shape in
    let n = 36 in (* arbitrary *)
    let points =
      MyExt.ListExt.linspace 0.0 (2.0 *. Shape.pi) (n - 1)
      |> List.map (Vector2D.mk_polar r)
      |> List.map (Vector2D.(+..) p) in
    draw (Shape.Polygon points)

and draw shape =
  match shape with
  | Shape.ShapeList lst -> List.iter draw lst
  | Shape.Point _      -> draw_point shape
  | Shape.Line _       -> draw_line shape
  | Shape.Polygon _    -> draw_polygon shape
  | Shape.Circle _     -> draw_circle shape
  | Shape.Recursive _  -> draw_recursive shape
