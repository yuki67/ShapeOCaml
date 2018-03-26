open JsExt

let (||>) v f = f v; v

let shape = Cardioid.shape
let n_range = Range.create ()
let k_range = Range.create ()
let n_span = Span.create ()
let k_span = Span.create ()
let n_br = Document.createElement "br"
let k_br = Document.createElement "br"

let refresh () =
  Cardioid.n := int_of_float (Range.get_value n_range);
  Cardioid.k := int_of_float (Range.get_value k_range);
  let str_n = string_of_int !Cardioid.n in
  let str_k = string_of_int !Cardioid.k in
  Span.set_text {j|n: $(str_n)|j} n_span;
  Span.set_text {j|n: $(str_k)|j} k_span

let wipe () =
  Range.hide n_range;
  Range.hide k_range;
  Span.hide n_span;
  Span.hide k_span;
  Element.hide n_br;
  Element.hide k_br

let init canvas redraw =
  n_range
  ||> Range.set_step 10.0
  ||> Range.set_max 400.0
  ||> Range.set_value 60.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  k_range
  ||> Range.set_max 9.0
  ||> Range.set_min 2.0
  ||> Range.set_value 2.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  Canvas.insertBefore canvas (n_range ||> Range.show);
  Canvas.insertBefore canvas (n_span ||> Span.show);
  Canvas.insertBefore canvas (n_br ||> Element.show);
  Canvas.insertBefore canvas (k_range ||> Range.show);
  Canvas.insertBefore canvas (k_span ||> Span.show);
  Canvas.insertBefore canvas (k_br ||> Element.show);

  refresh ()
