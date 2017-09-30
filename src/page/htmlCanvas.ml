open JsExt
open MyExt.ListExt
open Painter
open Vector2D
open Context

let (||>) v f = f v; v

let to_int_pair p = (int_of_float (get_x p)), (int_of_float (get_y p))

let draw_point ctx s =
  let x, y = to_int_pair (Shape.dest_point s) in
  ctx
  |> fillRect x y 1 1

let draw_line ctx s =
  let p, q = Shape.dest_line s in
  let a, b = to_int_pair p
  and c, d = to_int_pair q in
  ctx
  ||> beginPath
  ||> moveTo a b
  ||> lineTo c d
  ||> stroke
  |> ignore

let draw_polygon ctx s =
  let lst = Shape.dest_polygon s in
  if lst = [] then ()
  else
    let lst' = map to_int_pair lst in
    let a, b = hd lst' in
    ctx
    ||> beginPath
    ||> moveTo a b
    ||> (fun _ -> iter (fun (x, y) -> lineTo x y ctx) (tl lst'))
    ||> lineTo a b
    ||> stroke
    |> ignore

let draw_circle ctx s =
  let p, r = Shape.dest_circle s in
  let x, y = to_int_pair p
  and r' = int_of_float r in
  ctx
  |> arc x y r' 0.0 (2.0 *. Shape.pi ) (Js.Boolean.to_js_boolean false)

let init ctx =
  Drawer.add_drawer Shape.POINT (draw_point ctx);
  Drawer.add_drawer Shape.LINE (draw_line ctx);
  Drawer.add_drawer Shape.POLYGON (draw_polygon ctx);
  Drawer.add_drawer Shape.CIRCLE (draw_circle ctx)
