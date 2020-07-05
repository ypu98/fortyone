open Stdlib
open Card
open State
open Command

(* player 1 is the user, player 2 is the computer *)

(** [initialize f] initializes the game by creating an initial state *)
let init_state =
  distribute_cards Card.deck1
  |> state_of_result

let init_string st =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to a game of 41.\n")

let end_game st =
  if deck_of_state st = [] then ANSITerminal.(print_string [red] "\nThere are no cards left\n");

  ANSITerminal.(print_string [white] "\nThe game has ended. 
  \nHere are your final scores:");
  ANSITerminal.(print_string [white] "\n\nPlayer 1: "); 
  print_endline (string_of_int (score_of_hand (player1 st)));
  ANSITerminal.(print_string [white] "\nPlayer 2: ");
  print_endline (string_of_int (score_of_hand (player2 st)));

  if (score_of_hand (player1 st)) > (score_of_hand (player2 st))
  then ANSITerminal.(print_string [red] "\nYou've won!\n\n")
  else ANSITerminal.(print_string [red] "\nPlayer 2 has won!\n\n")

let rec user_input_command (st : State.t) (score : int) =

  if deck_of_state st = [] then end_game st else (

    ANSITerminal.(print_string [green] "\n\nCurrent hand: ");
    st |> string_of_hand |> print_endline;

    ANSITerminal.(print_string [green] "\n\nCurrent score: ");
    print_endline (string_of_int (score_of_hand (player1 st)));

    (* ANSITerminal.(print_string [blue] "\n\n[Debug Player2 Current hand:] \n");
       st |> debug_string_of_hand |> print_endline;

       ANSITerminal.(print_string [blue] "\n\n[Debug Player2 Current score:] ");
       print_endline (string_of_int (score_of_hand (player2 st))); *)

    ANSITerminal.(print_string [green] "\n\nDiscard pile: ");
    print_endline (peek_discard st);

    if List.length (player1_hand st) > 4 
    then ( ANSITerminal.(print_string [yellow]
                           "\n\nYou have more than 4 cards. Please discard one.\n") )
    else                           
      ANSITerminal.(print_string [yellow]
                      "\n\nWhat would you like to do?\n";);

    (if (List.length (player1_hand st) < 5 && score_of_hand (player1 st) > 36) 
     then ANSITerminal.(print_string [yellow]
                          "\n\nYour score is above 36. This is a good time to stop unless you want to keep playing.\n") 
     else ());

    print_endline "Please enter a command, either:";
    print_endline "draw deck | draw discard | discard <rank> of <suit> | stop\n";
    print_string  "> ";

    match read_line () with
    | exception End_of_file -> ()
    | command_input -> match (Command.parse command_input) with
      | exception (Malformed) -> ANSITerminal.(print_string [yellow]
                                                 "\n\nThat command is invalid\n");
        user_input_command st score

      | exception (Empty) -> ANSITerminal.(print_string [red]
                                             "\n\nYou have not entered a command.\n");
        user_input_command st score

      | Stop ->  ANSITerminal.(print_string [red]
                                 "\n\nYou have quit the game.\n"); end_game st

      | Draw pile -> if List.length (player1_hand st) > 4 
        then ( ANSITerminal.(print_string [red]
                               "\n\nYou must discard one card.\n");
               user_input_command st score)
        else ( match pile with
            | "deck" -> ( match draw_deck st (player1 st) with
                | Illegal -> ANSITerminal.(print_string [red]
                                             "\n\nThere is nothing to draw.\n");
                  user_input_command st score
                | Legal new_st -> ANSITerminal.(print_string [yellow]
                                                  "\n\nYou have drawn: ");
                  ANSITerminal.(print_string [yellow]
                                  (peek_deck st));
                  user_input_command new_st (score_of_hand (player1 new_st)))

            | "discard" -> ( match draw_discard st (player1 st) with
                | Illegal -> ANSITerminal.(print_string [red]
                                             "\n\nThere is nothing to draw.\n");
                  user_input_command st score
                | Legal new_st -> ANSITerminal.(print_string [yellow]
                                                  "\n\nYou have drawn: ");
                  ANSITerminal.(print_string [yellow]
                                  (peek_discard st));
                  user_input_command new_st (score_of_hand (player1 new_st)))

            | _ -> ANSITerminal.(print_string [yellow]
                                   "\n\nThat command is invalid\n"); 
              user_input_command st score
          )

      | Discard card -> 
        if List.length (player1_hand st) < 5 then 
          ( ANSITerminal.(print_string [red]
                            "\n\nYou must draw one card before you discard.\n");
            user_input_command st score)
        else (
          match (discard st (player1 st) (card_of_cmd card)) with
          | exception(NoSuchCard) -> ANSITerminal.(print_string [red]
                                                     "\n\nThat is not a valid card\n"); 
            user_input_command st score
          (* note: we tried to create a smarter algorithm for player 2's move
             but it took too much time to execute. to see the smart move in action,
             change [player2_move] to [player2_smart_move]. *)
          | Legal new_st -> let state_after_player2 = 
                              state_of_result (player2_move new_st) in
            user_input_command state_after_player2 
              (score_of_hand (player1 state_after_player2))
          | Illegal -> ANSITerminal.(print_string [red]
                                       "\n\nThat card is not in your hand.\n"); 
            user_input_command st score)

    (* | Discard card -> 
       match (discard st (player1 st) (card_of_cmd card)) with
       | Legal new_st -> user_input_command new_st (score_of_hand (player1 new_st))
       | Illegal -> ANSITerminal.(print_string [yellow]
                                   "\n\nThat card is not in your hand.\n"); 
        user_input_command st score *)
  )

(** [play_game] starts the game. *)
let play_game =
  init_string init_state; user_input_command (init_state) 
    (score_of_hand (player1 init_state))


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game

(* Execute the game engine. *)
let () = main ()