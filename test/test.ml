open Battleship
open OUnit2

let ship_logo_test (name : string) (ship_type : Battleship.ship_type)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (ship_logo ship_type)

let carrier = Carrier
let tests = "test suite for sum" >::: [ ship_logo_test "carrier" carrier " A " ]
let _ = run_test_tt_main tests
