exception Iter_stop

type primitive_shape =
  | POINT
  | LINE
  | POLYGON
  | CIRCLE

type t =
  | Point of Vector2D.t
  | Line of Vector2D.t * Vector2D.t
  | Polygon of Vector2D.t list
  | Circle of Vector2D.t * float
  | Recursive of (t -> int -> t) * t * int
  | ShapeList of t list

val pi : float

val dest_point : t -> Vector2D.t
val dest_shapelist : t -> t list
val dest_line : t -> Vector2D.t * Vector2D.t
val dest_polygon : t -> Vector2D.t list
val dest_circle : t -> Vector2D.t * float
val dest_recursive : t -> (t -> int -> t) * t * int

val regular_polygon : Vector2D.t -> int -> float -> t
val regular_polygon_array : Vector2D.t -> int -> float -> Vector2D.t array

val map_on_points : (Vector2D.t -> Vector2D.t) -> t -> t
val enlarge : Vector2D.t -> float -> t -> t
val move : Vector2D.t -> t -> t
