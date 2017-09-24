type t
val acceptable_error : float

val mk : float -> float -> t
val mki : int -> int -> t
val mk_polar : float -> float -> t

val eq : t -> t -> bool
val eq_exact : t -> t -> bool

val dest : t -> float * float
val get_x : t -> float
val get_y : t -> float

val (+..) : t -> t -> t
val (-..) : t -> t -> t
val ( *.. ) : float -> t -> t

val lerp : t -> t -> float -> t

val string_of_vector2D : t -> string
val print_vector2D : t -> unit
