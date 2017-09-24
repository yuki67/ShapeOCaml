type t = (float * float)

let acceptable_error = float_of_string "1.0e-6"

let mk x y = (x, y)
let mki x y = (float_of_int x, float_of_int y)
let mk_polar r theta = (r *. cos theta), (r *. sin theta)

let eq (a, b) (c, d) =
  abs_float (a -. c) < acceptable_error &&
  abs_float (b -. d) < acceptable_error
let eq_exact (a, b) (c, d) = a = c && b = d

let dest v = v
let get_x (x, _) = x
let get_y (_, y) = y

let (+..) (x, y) (a, b) = (x +. a, y +. b)
let (-..) (x, y) (a, b) = (x -. a, y -. b)
let ( *.. ) k (x, y) = (k *. x, k *. y)

let lerp a b r =
  a +.. (r *.. (b -.. a))

let string_of_vector2D v =
  "(" ^ (string_of_float (get_x v)) ^ ", " ^ (string_of_float (snd v)) ^ ")"

let print_vector2D v =
  print_string (string_of_vector2D v)
