open Painter
open Painter.Vector2D

let to_int_pair p = (int_of_float (get_x p)), (int_of_float (get_y p))

let draw_point s =
  let x, y = to_int_pair (Shape.dest_point s) in
  Graphics.fill_rect x y 1 1

let draw_line s =
  let p, q = Shape.dest_line s in
  Graphics.draw_poly_line [| to_int_pair p; to_int_pair q |]

let draw_polygon s =
  let lst = Shape.dest_polygon s in
  Graphics.draw_poly (Array.of_list (List.map to_int_pair lst))

let draw_circle s =
  let p, r = Shape.dest_circle s in
  let x, y = to_int_pair p
  and r' = int_of_float r in
  Graphics.draw_circle x y r'

let init () =
  Drawer.add_drawer Shape.POINT draw_point;
  Drawer.add_drawer Shape.LINE draw_line;
  Drawer.add_drawer Shape.POLYGON draw_polygon;
  Drawer.add_drawer Shape.CIRCLE draw_circle;
  Graphics.resize_window 512 512

let start_tour () =
  List.iter
    (fun shape ->
       Graphics.clear_graph ();
       shape ()
       |> Shape.enlarge (mk 0.0 0.0) 500.0
       |> Shape.move (mk 6.0 6.0)
       |> Drawer.draw;
       print_endline "press enter to draw next shape.";
       ignore (input_line stdin))
    Gallery.exhibitions;
  Graphics.clear_graph
