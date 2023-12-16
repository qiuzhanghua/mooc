open OUnit2
open Mooc

let tests =
  "test suite for math"
  >::: [
         ("euclid" >:: fun _ -> assert_equal 3 (Math.euclid 9 3));
         ( "invalid of Euclid" >:: fun _ ->
           assert_raises (Invalid_argument "euclid") (fun () -> Math.euclid 0 3)
         );
         ("ackermann" >:: fun _ -> assert_equal 125 (Math.ackermann 3 4));
       ]

let _ = run_test_tt_main tests
