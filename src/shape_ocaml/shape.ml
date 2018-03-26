exception Iter_stop

let pi =  3.14159265358979312

type point = Vector2D.t

type primitive_shape =
  | POINT
  | LINE
  | POLYGON
  | CIRCLE

type t =
  | Point of point
  | Line of point * point
  | Polygon of point list
  | Circle of point * float
  | Recursive of (t -> int -> t) * t * int
  | ShapeList of t list

let dest_point = function
  | Point p -> p
  | _ -> failwith "dest_point"

let dest_shapelist = function
  | ShapeList s -> s
  | _ -> failwith "dest_shapelist"

let dest_line = function
  | Line (p, q) -> p, q
  | _ -> failwith "dest_line"

let dest_polygon = function
  | Polygon lst -> lst
  | _ -> failwith "dest_polygon"

let dest_circle = function
  | Circle (p, r) -> p, r
  | _ -> failwith "dest_circle"

let dest_recursive = function
  | Recursive (gen, init, n) -> gen, init, n
  | _ -> failwith "dest_recursive"

let regular_polygon center n r =
  let open Vector2D in
  let open MyExt in
  let offset = 3.0 *. pi /. (float_of_int n) +. 0.5 *. pi in
  let points =
    ListExt.take (ListExt.linspace 0.0 (2.0 *. pi) (n + 1)) n
    |> List.map (fun x -> x -. offset)
    |> List.map (fun theta -> center +.. (mk_polar r (theta -. offset))) in
  Polygon points

open MyExt.ListExt

let regular_polygon_array center n r =
  let open Vector2D in
  let offset = 3.0 *. pi /. (float_of_int n) +. 0.5 *. pi in
  Array.map (fun theta -> center +.. (mk_polar r (theta -. offset)))
    (Array.of_list (take (linspace 0.0 (2.0 *. pi) (n + 1)) n))

let rec map_on_points f = function
  | Point p -> Point (f p)
  | Line (p, q) -> Line (f p, f q)
  | Polygon plist -> Polygon (map f plist)
  | Circle (p, r) -> Circle (f p, r)
  | Recursive (gen, init, n) -> Recursive (gen, map_on_points f init, n)
  | ShapeList lst -> ShapeList (map (map_on_points f) lst)

let enlarge center r =
  map_on_points (fun x -> Vector2D.lerp center x r)

let move v =
  let open Vector2D in
  map_on_points (fun x -> x +.. v)
