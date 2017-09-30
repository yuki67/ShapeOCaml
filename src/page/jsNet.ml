open JsExt
include Net

let (||>) v f = f v; v

let n_range = Range.create ()
let n_span = Span.create ()

let d_range = Range.create ()
let d_span = Span.create ()

let refresh () =
  let n' = int_of_float (Range.get_value n_range)
  and d' = Range.get_value d_range in
  n := n';
  d := d';
  let n_str = string_of_int n'
  and d_str = string_of_float d' in
  Span.set_text {j|n: $(n_str)|j} n_span;
  Span.set_text {j|d: $(d_str)|j} d_span

let load redraw =
  Range.set_oninput (fun _ -> refresh (); redraw ()) n_range;
  Range.set_oninput (fun _ -> refresh (); redraw ()) d_range;
  Range.show n_range;
  Range.show d_range;
  Span.show n_span;
  Span.show d_span

let wipe () =
  Range.hide d_range;
  Range.hide n_range;
  Span.hide d_span;
  Span.hide n_span

let _ =
  n_range
  ||> Range.set_max 8.0
  |> Range.set_value 1.0;

  d_range
  ||> Range.set_max 0.25
  ||> Range.set_min (-0.25)
  ||> Range.set_step 0.025
  |> Range.set_value 0.075;

  Document.just_put n_range;
  Document.just_put n_span;
  Document.new_line ();
  Document.just_put d_range;
  Document.just_put d_span;
  Document.new_line ();

  refresh ();
  wipe ()
