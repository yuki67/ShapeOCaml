open JsExt

let (||>) v f = f v; v

let shape = Net.shape
let n_range = Range.create ()
let d_range = Range.create ()
let n_span = Span.create ()
let d_span = Span.create ()
let n_br = Document.createElement "br"
let d_br = Document.createElement "br"

let refresh () =
  let n' = int_of_float (Range.get_value n_range)
  and d' = Range.get_value d_range in
  Net.n := n';
  Net.d := d';
  let n_str = string_of_int n'
  and d_str = string_of_float d' in
  Span.set_text {j|n: $(n_str)|j} n_span;
  Span.set_text {j|d: $(d_str)|j} d_span

let wipe () =
  Range.hide n_range;
  Range.hide d_range;
  Span.hide n_span;
  Span.hide d_span;
  Element.hide n_br;
  Element.hide d_br

let init canvas redraw =
  n_range
  ||> Range.set_max 8.0
  ||> Range.set_value 1.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  d_range
  ||> Range.set_max 0.25
  ||> Range.set_min (-0.25)
  ||> Range.set_step 0.025
  ||> Range.set_value 0.075
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  Canvas.insertBefore canvas (n_range ||> Range.show);
  Canvas.insertBefore canvas (n_span ||> Span.show);
  Canvas.insertBefore canvas (n_br ||> Element.show);
  Canvas.insertBefore canvas (d_range ||> Range.show);
  Canvas.insertBefore canvas (d_span ||> Span.show);
  Canvas.insertBefore canvas (d_br ||> Element.show);

  refresh ()
