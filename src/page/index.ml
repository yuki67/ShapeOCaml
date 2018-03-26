open JsExt

let (||>) v f = f v; v

let exhibitions =
  let ignore2 = fun _ _ -> () in
  [|
    (ignore2, (fun _ -> Shape.ShapeList []), ignore);
    (JsDiamond.init,
     JsDiamond.shape,
     JsDiamond.wipe);
    (JsNet.init,
     JsNet.shape,
     JsNet.wipe);
    (JsCardioid.init,
     JsCardioid.shape,
     JsCardioid.wipe);
  |]

let init, shape, wipe =
  let i, s, w = ArrayLabels.get exhibitions 0 in
  ref i, ref s, ref w

let load_exhibition n =
  let i, s, w = ArrayLabels.get exhibitions n in
  init := i;
  shape := s;
  wipe := w

let canvas_size = ref 512.0
let canvas = Canvas.create ()
let context = Canvas.getContext canvas "2d"
let enlarge_button = Button.create ()
let shrink_button = Button.create ()
let sel = Select.create ()

let refresh_canvas_size () =
  canvas
  ||> Canvas.set_width (int_of_float !canvas_size)
  |> Canvas.set_height (int_of_float !canvas_size)

let redraw () =
  let open Painter.Vector2D in
  Context.clearRect 0 0 (int_of_float !canvas_size) (int_of_float !canvas_size) context;
  !shape ()
  |> Shape.enlarge (mk 0.0 0.0) (!canvas_size -. 10.0)
  |> Shape.move (mk 5.0 5.0)
  |> Drawer.draw

let enlarge_canvas r () =
  canvas_size := !canvas_size *. r;
  refresh_canvas_size ();
  redraw ()

let _ =
  let current_script = Document.current_script in

  canvas
  ||> Canvas.set_width (int_of_float !canvas_size)
  |> Canvas.set_height (int_of_float !canvas_size);

  enlarge_button
  ||> Button.set_text "enlarge"
  |> Button.set_onclick (enlarge_canvas 1.1);

  shrink_button
  ||> Button.set_text "shrink"
  |> Button.set_onclick (enlarge_canvas 0.9);

  sel
  ||> Select.add_option {j|choose |j} 0
  ||> Select.add_option "Diamond" 1
  ||> Select.add_option "Net" 2
  ||> Select.add_option "Cardioid" 3
  ||> Select.set_onchange
    (fun _ ->
       !wipe ();
       load_exhibition (Select.get_value sel);
       !init current_script redraw;
       redraw ())
  |> Document.just_put;

  Element.insertBefore current_script enlarge_button;
  Element.insertBefore current_script shrink_button;
  Element.insertBefore current_script (Document.createElement "br");
  Element.insertBefore current_script canvas;

  HtmlCanvas.init context;
  ()
