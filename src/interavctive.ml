open Vector2D

let to_int_pair p = (int_of_float (get_x p)), (int_of_float (get_y p))

let draw_point s =
  let x, y = to_int_pair (Shape.dest_point s) in
  Graphics.fill_rect x y 1 1

let draw_line s =
  let p, q = Shape.dest_line s in
  Graphics.draw_poly_line [| to_int_pair p; to_int_pair q |]

let draw_polygon s =
  let lst = Shape.dest_polygon s in
  Graphics.draw_poly_line (Array.of_list (List.map to_int_pair lst))

let draw_circle s =
  let p, r = Shape.dest_circle s in
  let (x, y) = to_int_pair p
  and r' = int_of_float r in
  Graphics.draw_circle x y r'

let init () =
  Shape.add_drawer Shape.POINT draw_point;
  Shape.add_drawer Shape.LINE draw_line;
  Shape.add_drawer Shape.POLYGON draw_polygon;
  Shape.add_drawer Shape.CIRCLE draw_circle
