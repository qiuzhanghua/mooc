open OUnit2
open Mooc
open Test_utils

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
         ( "sum3" >:: fun _ ->
           assert_equal 40 (Math.sum3 3) ~printer:string_of_int );
         ( "tribonacci" >:: fun _ ->
           assert_equal 17 (Math.tribonacci 7 1 1 1) ~printer:string_of_int );
         ( "fast_exp" >:: fun _ ->
           assert_equal 512 (Math.fast_exp 2 9) ~printer:string_of_int );
         ( "catalan" >:: fun _ ->
           assert_equal 132 (Math.catalan 6) ~printer:string_of_int );
         ( "mccarthy" >:: fun _ ->
           assert_equal 190 (Math.mccarthy 200) ~printer:string_of_int );
         ( "triangle" >:: fun _ ->
           assert_equal 2 (Math.triangle 3) ~printer:string_of_int );
         ( "sum" >:: fun _ ->
           assert_equal 9 (Math.triangle 9) ~printer:string_of_int );
         ( "collatz 1" >:: fun _ ->
           assert_equal [ 1 ] (Math.collatz 1) ~printer:string_of_list );
         ( "collatz 2" >:: fun _ ->
           assert_equal [ 2; 1 ] (Math.collatz 2) ~printer:string_of_list );
         ( "collatz 6" >:: fun _ ->
           let e : int list = [ 6; 3; 10; 5; 16; 8; 4; 2; 1 ] in
           assert_equal e (Math.collatz 6) ~printer:string_of_list );
       ]

let _ = run_test_tt_main tests
