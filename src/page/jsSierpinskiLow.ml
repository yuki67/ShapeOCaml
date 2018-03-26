open JsExt

let (||>) v f = f v; v

let shape = Sierpinski.shape
let n_range = Range.create ()
let n_span = Span.create ()
let n_br = Document.createElement "br"

let refresh () =
  Sierpinski.n := int_of_float (Range.get_value n_range);
  let str_n = string_of_int !Sierpinski.n in
  Span.set_text {j|n: $(str_n)|j} n_span

let wipe () =
  Range.hide n_range;
  Span.hide n_span;
  Element.hide n_br

let init canvas redraw =
  n_range
  ||> Range.set_min 1.0
  ||> Range.set_max 8.0
  ||> Range.set_step 1.0
  ||> Range.set_value 2.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  Canvas.insertBefore canvas (n_range ||> Range.show);
  Canvas.insertBefore canvas (n_span ||> Span.show);
  Canvas.insertBefore canvas (n_br ||> Element.show);

  refresh ()
