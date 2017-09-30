open JsExt

include Diamond

let (||>) v f = f v; v

let n_range = Range.create ()
let n_span = Span.create ()

let refresh () =
  let k = int_of_float (Range.get_value n_range) in
  n := k;
  let str = string_of_int k in
  Span.set_text {j|n: $(str)|j} n_span

let load redraw =
  Range.set_oninput (fun _ -> refresh (); redraw ()) n_range;
  Range.show n_range;
  Span.show n_span

let wipe () =
  Range.hide n_range;
  Span.hide n_span

let _ =
  n_range
  ||> Range.set_max 64.0
  |> Range.set_value 16.0;

  Document.just_put n_range;
  Document.just_put n_span;
  Document.new_line ();

  refresh ();
  wipe()
