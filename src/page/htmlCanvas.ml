open JsExt
open Painter
open Vector2D

let (||>) v f = f v; v

let to_int_pair p = (int_of_float (get_x p)), (int_of_float (get_y p))

let draw_point ctx s =
  let x, y = to_int_pair (Shape.dest_point s) in
  ctx
  |> Context.fillRect x y 1 1

let draw_line ctx s =
  let p, q = Shape.dest_line s in
  let a, b = to_int_pair p
  and c, d = to_int_pair q in
  ctx
  ||> Context.beginPath
  ||> Context.moveTo a b
  ||> Context.lineTo c d
  |> Context.stroke

let draw_polygon ctx s =
  let module List = MyExt.ListExt in
  let lst = Shape.dest_polygon s in
  if lst = [] then ()
  else
    let lst' = List.map to_int_pair lst in
    let a, b = List.hd lst' in
    ctx
    ||> Context.beginPath
    ||> Context.moveTo a b
    ||> (fun _ ->
        List.iter
          (fun (x, y) -> Context.lineTo x y ctx) (List.tl lst'))
    ||> Context.lineTo a b
    |> Context.stroke

let draw_circle ctx s =
  let p, r = Shape.dest_circle s in
  let x, y = to_int_pair p
  and r' = int_of_float r in
  ctx
  |> Context.arc x y r' 0.0 (2.0 *. Shape.pi) (Boolean.to_js_boolean false)

let init ctx =
  Drawer.add_drawer Shape.POINT (draw_point ctx);
  Drawer.add_drawer Shape.LINE (draw_line ctx);
  Drawer.add_drawer Shape.POLYGON (draw_polygon ctx);
  Drawer.add_drawer Shape.CIRCLE (draw_circle ctx)
