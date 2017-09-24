open OUnit
open MyExt
open ListExt
open Painter
open Vector2D

let (@=) u v = assert_equal ~cmp:eq u v
let (@!=) u v = assert_equal ~cmp:(fun x y -> not (eq x y)) u v

let vec_basic () =

  mk 1.0 2.0 @= mk 1.0 2.0;
  mki 1 2 @= mk 1.0 2.0;

  assert_equal (get_x (mk 1.0 2.0)) 1.0;
  assert_equal (get_y (mk 1.0 2.0)) 2.0;

  mk 1.00001 2. @!= mk 1. 2.;
  mk (1.0 +. acceptable_error *. 0.9) 2. @= mk 1. 2.;

  let p1, p2 = mk 3.3 4.4,  mk ~-.1.1 5.5 in
  p1 +.. p2 @= mk 2.2 9.9;
  p1 -.. p2 @= mk 4.4 ~-.1.1;
  3.0 *.. p1 @= mk 9.9 13.2;
  ()

let vec_func () =
  let p1, p2 = mk 3.3 4.4,  mk ~-.1.1 5.5 in
  lerp p1 p2 0.5 @= mk 1.1 4.95;
  iter
    (fun r -> lerp p1 p2 r @= lerp p2 p1 (1.0 -. r))
    (linspace 0.0 1.0 10);


  ()


let main =
  "suite" >::: [
    "vector2D basics" >:: vec_basic;
    "vector2D function" >:: vec_func
  ]

let _ =
  run_test_tt_main main
