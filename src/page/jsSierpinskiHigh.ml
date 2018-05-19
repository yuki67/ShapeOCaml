open JsExt

let (||>) v f = f v; v

let shape = Sierpinski.shape
let n_range = Range.create ()
let m_range = Range.create ()
let d_range = Range.create ()
let n_span = Span.create ()
let m_span = Span.create ()
let d_span = Span.create ()
let n_br = Document.createElement "br"
let m_br = Document.createElement "br"
let d_br = Document.createElement "br"

let refresh () =
  Sierpinski.n := int_of_float (Range.get_value n_range);
  Sierpinski.m := int_of_float (Range.get_value m_range);
  Sierpinski.d := Range.get_value d_range;
  let str_n = string_of_int !Sierpinski.n in
  let str_k = string_of_int !Sierpinski.m in
  let str_d = string_of_float !Sierpinski.d in
  Span.set_text {j|n: $(str_n)|j} n_span;
  Span.set_text {j|m: $(str_k)|j} m_span;
  Span.set_text {j|d: $(str_d)|j} d_span

let wipe () =
  Range.hide n_range;
  Range.hide m_range;
  Range.hide d_range;
  Span.hide n_span;
  Span.hide m_span;
  Span.hide d_span;
  Element.hide n_br;
  Element.hide m_br;
  Element.hide d_br

let init canvas redraw =
  Sierpinski.n := 2;
  Sierpinski.m := 3;
  Sierpinski.d := 0.5;

  n_range
  ||> Range.set_min 1.0
  ||> Range.set_max 2.0
  ||> Range.set_step 1.0
  ||> Range.set_value 2.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  m_range
  ||> Range.set_min 3.0
  ||> Range.set_max 32.0
  ||> Range.set_step 1.0
  ||> Range.set_value 3.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  d_range
  ||> Range.set_min 0.0
  ||> Range.set_max 1.0
  ||> Range.set_step 0.025
  ||> Range.set_value 0.5
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  Canvas.insertBefore canvas (n_range ||> Range.show);
  Canvas.insertBefore canvas (n_span ||> Span.show);
  Canvas.insertBefore canvas (n_br ||> Element.show);
  Canvas.insertBefore canvas (m_range ||> Range.show);
  Canvas.insertBefore canvas (m_span ||> Span.show);
  Canvas.insertBefore canvas (m_br ||> Element.show);
  Canvas.insertBefore canvas (d_range ||> Range.show);
  Canvas.insertBefore canvas (d_span ||> Span.show);
  Canvas.insertBefore canvas (d_br ||> Element.show);

  refresh ()
