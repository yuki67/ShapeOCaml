open JsExt

let (||>) v f = f v; v

let shape = Diamond.shape
let n_range = Range.create ()
let n_span = Span.create ()
let n_br = Document.createElement "br"

let refresh () =
  let k = int_of_float (Range.get_value n_range) in
  let str = string_of_int k in
  Diamond.n := k;
  Span.set_text {j|n: $(str)|j} n_span

let wipe () =
  Range.hide n_range;
  Span.hide n_span;
  Element.hide n_br

let init current_script redraw =
  n_range
  ||> Range.set_max 64.0
  ||> Range.set_value 16.0
  |> Range.set_oninput (fun _ -> refresh (); redraw ());

  Element.insertBefore current_script (n_range ||> Range.show);
  Element.insertBefore current_script (n_span ||> Span.show);
  Element.insertBefore current_script (n_br ||> Element.show);

  refresh ()
