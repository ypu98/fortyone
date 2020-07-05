(** 
   Representation of dynamic game state.

   This module represents the state of the card game as it is being played,
   including the players’ current hands, the cards that have been discarded,
   the cards left in the stack, and functions that change the state of the game.
*)

(** The abstract type of values representing the game state. *)
type t 

type player

(** [init_state deck] is the initial state of the game when playing 
    the card game. *)
val init_state : Card.card list -> t

(** [current_hand t player] is the set of cards a player has at a point in the
    card game *)
val current_hand : t -> player -> Card.card list

(** [player1_hand st] is the set of cards player 1 has at a point in the
    card game. *)
val player1_hand : t -> Card.card list
val player2_hand : t -> Card.card list

(** [top_of_discard_pile state] is the card that is at the top of the discard
    pile at a given state in the game. *)
val top_of_discard_pile : t -> Card.card

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [draw_discard st player] is [r] if attempting to draw from the discard pile
    in state [st] results in [r]. If discard pile is nonempty, then [r] is
    [Legal st'], where in [st'] the players hands are updated.
    Otherwise, the result is [Illegal]. *)
val draw_discard : t -> player -> result

(** [draw_deck st player] is [r] if attempting to draw from the deck in
    state [st] results in [r]. If deck is nonempty, then [r] is
    [Legal st'], where in [st'] the players hands are updated.
    Otherwise, the result is [Illegal]. *)
val draw_deck : t -> player -> result

(** [discard st player card] is [r] if attempting to discard a card from a player's
    hand in state [st] results in [r]. If [card] is in player's hand, then [r]
    is [Legal st'], where in [st'] the players hands and discard pile are 
    updated. Otherwise, the result is [Illegal]. *)
val discard : t -> player -> Card.card -> result

(** [distribute_cards cards] distributes cards to each player. *)
val distribute_cards : Card.card list -> result

(** [state_of_result result] is the state of a given result. *)
val state_of_result : result -> t

(** [string_of_hand st] is the string of player 1's cards *)
val string_of_hand : t -> string

(** [player1 st] is player 1 of the state. *)
val player1 : t -> player

(** [score_of_hand player] is the score of a player. *)
val score_of_hand : player -> int

(** [peek_discard st] is the card at the top of the discard pile. *)
val peek_discard : t -> string

(** [peek_deck st] is the card at the top of the deck. *)
val peek_deck : t -> string

(** [player2 st] is player 2 of a state. *)
val player2 : t -> player

(** smart move made by the computer *)
val player2_smart_move : t -> result

(** one move made by the computer
    always draw from the deck (if no card in the deck, illegal)
    then discard first card in hand *)
val player2_move : t -> result

(** [debug_string_of_hand st] is the string of player 2's cards. Used for
    testing purposes. *)
val debug_string_of_hand : t -> string

(** [debug_player2_hand st] is player 2's cards. Used for testing. *)
val debug_player2_hand : t -> Card.card list

(** [deck_of_state st] is the deck of a given state. *)
val deck_of_state : t -> Card.card list

(** [player2_not_very_smart_move st] is a move by player2 in the game. *)
val player2_not_very_smart_move : t -> result