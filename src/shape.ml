open MyExt
open ListExt
open Vector2D

exception Iter_stop

type point = Vector2D.t
type primitive_shape = POINT | LINE | POLYGON | CIRCLE
type t =
  | Point of point
  | Line of point * point
  | Polygon of point list
  | Circle of point * float
  | Recursive of (t -> int -> t) * t * int
  | ShapeList of t list

let dest_point = function
  | Point p -> p        | _ -> failwith "dest_point"
let dest_shapelist = function
  | ShapeList s -> s    | _ -> failwith "dest_shapelist"
let dest_line = function
  | Line (p, q) -> p, q | _ -> failwith "dest_line"
let dest_polygon = function
  | Polygon lst -> lst  | _ -> failwith "dest_polygon"
let dest_circle = function
  | Circle (p, r) -> p, r | _ -> failwith "dest_circle"
let dest_recursive = function
  | Recursive (gen, init, n) -> gen, init, n | _ -> failwith "dest_recursive"

let pi =  3.14159265358979312

let drawer_alist =
  ref [ (POINT, None);
        (LINE, None);
        (POLYGON, None);
        (CIRCLE, None)]

let add_drawer prim_type f =
  drawer_alist := (prim_type, Some f) :: !drawer_alist

let regular_polygon center n r =
  let offset = 3.0 *. pi /. (float_of_int n) -. 0.5 *. pi in
  Polygon
    (map (fun theta -> center +.. (mk_polar r (theta -. offset)))
       (take (linspace 0.0 (2.0 *. pi) (n + 1)) n))

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

and draw = function
  | ShapeList lst -> iter draw lst
  | Point _     as s -> draw_point s
  | Line _      as s -> draw_line s
  | Polygon _   as s -> draw_polygon s
  | Circle _    as s -> draw_circle s
  | Recursive _ as s -> draw_recursive s

let rec map_on_points f = function
  | Point p -> Point (f p)
  | Line (p, q) -> Line (f p, f q)
  | Polygon plist -> Polygon (map f plist)
  | Circle (p, r) -> Circle (f p, r)
  | Recursive (gen, init, n) -> Recursive (gen, map_on_points f init, n)
  | ShapeList lst -> ShapeList (map (map_on_points f) lst)

let rec enlarge center r = map_on_points (fun x -> lerp center x r)

let rec move v = map_on_points (fun x -> x +.. v)

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

let shrink_to_points ?(r=0.5) poly n =
  let lst = dest_polygon poly in
  enlarge (nth lst n) r poly
