open OUnit2
open Card
open Command
open State

(** TESTING REPORT
    What we tested:
    We mainly tested parser functions and functions that converted one data type
    to another, using OUnit. Our other functionality were tested through
    gameplay, i.e. manually instead of OUnit.

    What we omitted testing:
    We omitted testing helper functions directly and instead tested the
    functions that used them or tested them through gameplay.

    Why we believe that our test suite demonstrates the correctness of our system:
    As we have seen in A2/A3, a lot of the functionality of the game itself
    is tested through gameplay. Of course, rigorous testing of the functions
    in OUnit test cases are equally important. *)

let card_tests = [
  "test card_of_cmd" >:: 
  (fun _ -> assert_equal ("Queen of Hearts") (card_to_string (card_of_cmd ["queen"; "of"; "hearts"])));
  "test card_of_cmd 2" >:: 
  (fun _ -> assert_equal ("Queen of Hearts") (card_to_string (card_of_cmd ["queen"; "of"; "Hearts"]))); 
  "test card_of_cmd 3" >:: 
  (fun _ -> assert_equal ("Queen of Hearts") (card_to_string (card_of_cmd ["Queen"; "of"; "Hearts"])));
]

let command_tests = [
  "test Draw" >:: 
  (fun _ -> assert_equal (Draw "deck") (parse "    draw  deck   "));
  "test Draw" >:: 
  (fun _ -> assert_equal (Draw "discard") (parse "draw     discard"));
  "test Discard" >:: 
  (fun _ -> assert_equal (Discard ["queen"; "of"; "hearts"]) (parse "discard queen of hearts"));
  "test Stop" >:: (fun _ -> assert_equal Stop (parse "stop"));
  "test Malformed" >:: (fun _ -> assert_raises Malformed (fun () -> parse "stop h"));
  "test Empty" >:: (fun _ -> assert_raises Empty (fun () -> parse ""));
]

let state_tests = [
  (* player 2 tests *)

]

let suite =
  "test suite for fortyone"  >::: List.flatten [
    card_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite