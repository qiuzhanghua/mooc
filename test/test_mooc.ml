open OUnit2

let tests =
  "test suite for math" >::: [ ("hello" >:: fun _ -> assert_equal 2 (1 + 1)) ]

let _ = run_test_tt_main tests
