open MyExt.ListExt
open Vector2D
open Shape

(* no exhoustiveness check. *)
let drawer_alist =
  ref [ (POINT, None);
        (LINE, None);
        (POLYGON, None);
        (CIRCLE, None)]

let add_drawer prim_type f =
  drawer_alist := (prim_type, Some f) :: !drawer_alist

let rec draw_point shape =
  match assoc POINT !drawer_alist with
  | Some drawer -> drawer shape
  | None -> failwith "No way to draw a shape!"

and draw_line shape =
  match assoc LINE !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
      let p, q = dest_line shape in
      let points = map (fun r -> Point (lerp p q r)) (linspace 0.0 1.0 50) in
      iter draw points

and draw_polygon shape =
  match assoc POLYGON !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
      let lst = dest_polygon shape in
      let lines = loop_pairs (fun p q -> Line (p, q)) lst in
      iter draw lines

and draw_recursive shape =
  let gen, init, n = dest_recursive shape in
  let rec loop shape n =
    if n = 0 then draw shape
    else
      let gen' = gen shape in
      let rec loop2 i =
        loop (gen' i) (n - 1);
        loop2 (i + 1) in
      try loop2 0 with Iter_stop -> () in
  loop init n

and draw_circle shape =
  match assoc CIRCLE !drawer_alist with
  | Some drawer -> drawer shape
  | None ->
      let p, r = dest_circle shape in
      let n = 36 in (* arbitrary *)
      let points =
        linspace 0.0 (2.0 *. pi) (n - 1)
        |> map (mk_polar r)
        |> map ((+..) p) in
      draw (Polygon points)

and draw shape =
  match shape with
  | ShapeList lst -> iter draw lst
  | Point _      -> draw_point shape
  | Line _       -> draw_line shape
  | Polygon _    -> draw_polygon shape
  | Circle _     -> draw_circle shape
  | Recursive _  -> draw_recursive shape
