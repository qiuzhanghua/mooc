open OUnit2
open Mooc
open Test_utils

let tests =
  "test suite for List"
  >::: [
         ( "halve" >:: fun _ ->
           assert_equal
             ([ 1; 2 ], [ 3; 4; 5 ])
             (Lst.halve [ 1; 2; 3; 4; 5 ])
             ~printer:string_of_list_pair );
       ]

let _ = run_test_tt_main tests
