open JsExt

include Cardioid

let (||>) v f = f v; v

let n_range = Range.create ()
let n_span = Span.create ()
let k_range = Range.create ()
let k_span = Span.create ()

let refresh () =
  let n' = int_of_float (Range.get_value n_range) in
  let k' = int_of_float (Range.get_value k_range) in
  n := n';
  k := k';
  let str_n = string_of_int n' in
  let str_k = string_of_int k' in
  Span.set_text {j|n: $(str_n)|j} n_span;
  Span.set_text {j|n: $(str_k)|j} k_span

let load redraw =
  Range.set_oninput (fun _ -> refresh (); redraw ()) n_range;
  Range.set_oninput (fun _ -> refresh (); redraw ()) k_range;
  Range.show n_range;
  Range.show k_range;
  Span.show n_span;
  Span.show k_span

let wipe () =
  Range.hide n_range;
  Range.hide k_range;
  Span.hide n_span;
  Span.hide k_span

let _ =
  n_range
  ||> Range.set_step 10.0
  ||> Range.set_max 400.0
  |> Range.set_value 60.0;

  k_range
  ||> Range.set_max 9.0
  ||> Range.set_min 2.0
  |> Range.set_value 2.0;

  Document.just_put n_range;
  Document.just_put n_span;
  Document.new_line ();
  Document.just_put k_range;
  Document.just_put k_span;
  Document.new_line ();

  refresh ();
  wipe()
