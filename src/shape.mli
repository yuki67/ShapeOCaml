type primitive_shape = POINT | LINE | POLYGON | CIRCLE
type t =
  | Point of Vector2D.t
  | Line of Vector2D.t * Vector2D.t
  | Polygon of Vector2D.t list
  | Circle of Vector2D.t * float
  | Recursive of (t -> int -> t) * t * int
  | ShapeList of t list
val pi : float
val drawer_alist : (primitive_shape * (t -> unit) option) list ref

val dest_point : t -> Vector2D.t
val dest_shapelist : t -> t list
val dest_line : t -> Vector2D.t * Vector2D.t
val dest_polygon : t -> Vector2D.t list
val dest_circle : t -> Vector2D.t * float
val dest_recursive : t -> (t -> int -> t) * t * int

val add_drawer : primitive_shape -> (t -> unit) -> unit
val draw : t -> unit
val regular_polygon : Vector2D.t -> int -> float -> t

val map_on_points : (Vector2D.t -> Vector2D.t) -> t -> t
val enlarge : Vector2D.t -> float -> t -> t
val move : Vector2D.t -> t -> t

val square_subtransform : (Vector2D.t -> Vector2D.t) -> int -> t
val shrink_to_points : ?r:float -> t -> int -> t
