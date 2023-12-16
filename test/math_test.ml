open OUnit2
open Mooc

let string_of_triple (a, b, c) = Printf.sprintf "(%d, %d, %d)" a b c
let string_of_pair (a, b) = Printf.sprintf "(%d, %d)" a b

let tests =
  "test suite for math"
  >::: [
         ("euclid" >:: fun _ -> assert_equal 3 (Math.euclid 9 3));
         ( "invalid of Euclid" >:: fun _ ->
           assert_raises (Invalid_argument "euclid") (fun () -> Math.euclid 0 3)
         );
         ( "ackermann" >:: fun _ ->
           assert_equal 125 (Math.ackermann 3 4) ~printer:string_of_int );
         ( "approximate_pi" >:: fun _ ->
           if cmp_float Float.pi (Math.approximate_pi 100000) ~epsilon:1.0e-4
           then ()
           else assert_failure "approximate_pi" );
         ( "fail_with_pi" >:: fun _ ->
           assert_raises (Failure "approximate_pi") (fun () ->
               Math.approximate_pi 100) );
         ( "digits" >:: fun _ ->
           assert_equal (2, 36, 10) (Math.digits 1073741823 3)
             ~printer:string_of_triple );
         (* ("hhq" >:: fun _ ->
            assert_equal 7 (Math.hhq 1 4 12)); *)
         ( "hfm" >:: fun _ ->
           assert_equal (5, 4) (Math.hfm 7) ~printer:string_of_pair );
       ]

let _ = run_test_tt_main tests
