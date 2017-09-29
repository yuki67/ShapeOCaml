val drawer_alist : (Shape.primitive_shape * (Shape.t -> unit) option) list ref
val add_drawer : Shape.primitive_shape -> (Shape.t -> unit) -> unit
val draw : Shape.t -> unit
