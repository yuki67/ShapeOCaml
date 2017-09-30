open JsExt
open Painter
open Vector2D

let (||>) v f = f v; v

let exhibitions =
  [|
    (ignore, (fun _ -> Shape.ShapeList []), ignore);
    (JsDiamond.load, JsDiamond.shape, JsDiamond.wipe);
    (JsNet.load, JsNet.shape, JsNet.wipe);
    (JsCardioid.load, JsCardioid.shape, JsCardioid.wipe);
  |]

let load, shape, wipe =
  let l, s, w = ArrayLabels.get exhibitions 0 in
  ref l, ref s, ref w

let load_exhibition n =
  let l, s, w = ArrayLabels.get exhibitions n in
  load := l;
  shape := s;
  wipe := w

let size = ref 512.0
let canvas = Canvas.create ()
let context = Canvas.getContext canvas "2d"
let enlarge_button = Button.create ()
let shrink_button = Button.create ()
let sel = Select.create ()

let refresh_canvas_size () =
  canvas
  ||> Canvas.set_width (int_of_float !size)
  |> Canvas.set_height (int_of_float !size)

let redraw () =
  Context.clearRect 0 0 (int_of_float !size) (int_of_float !size) context;
  !shape ()
  |> Shape.enlarge (mk 0.0 0.0) (!size -. 10.0)
  |> Shape.move (mk 5.0 5.0)
  |> Drawer.draw

let enlarge_canvas r () =
  size := !size *. r;
  refresh_canvas_size ();
  redraw ()

let _ =
  canvas
  ||> Canvas.set_width (int_of_float !size)
  |> Canvas.set_height (int_of_float !size);

  enlarge_button
  ||> Button.set_text "enlarge"
  |> Button.set_onclick (enlarge_canvas 1.1);

  shrink_button
  ||> Button.set_text "shrink"
  |> Button.set_onclick (enlarge_canvas 0.9);

  sel
  ||> Select.add_option {j|図形を選択|j} 0
  ||> Select.add_option "Diamond" 1
  ||> Select.add_option "Net" 2
  ||> Select.add_option "Cardioid" 3
  ||> Select.set_onchange (fun _ ->
    !wipe ();
    load_exhibition (Select.get_value sel);
    !load redraw;
    redraw ())
  |> Document.just_put;

  Document.just_put enlarge_button;
  Document.just_put shrink_button;
  Document.just_put (Document.createElement "br");
  Document.just_put canvas;

  HtmlCanvas.init context;
  ()
