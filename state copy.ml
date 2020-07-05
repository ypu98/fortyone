open Card

type player = {
  name : string;
  cards : Card.card list;
}

type t = {
  player1 : player;
  player2 : player;
  deck : Card.card list;
  discard_pile : Card.card list;
}

type result = Legal of t | Illegal

let deck_of_state st = st.deck

let empty_state = {
  player1 = {
    name = "Player 1";
    cards = []
  };
  player2 = {
    name = "Player 2";
    cards = []
  };
  deck = [];
  discard_pile = [];
}

let draw_deck st player =
  match st.deck with
  | [] -> Illegal
  | h :: t -> if player.name = "Player 1" 
    then Legal ({
        player1 = {
          name = "Player 1";
          cards = h :: st.player1.cards
        };
        player2 = st.player2;
        deck = t;
        discard_pile = st.discard_pile;
      })
    else Legal ({
        player1 = st.player1;
        player2 = {
          name = "Player 2";
          cards = h :: st.player2.cards
        };
        deck = t;
        discard_pile = st.discard_pile;
      })

let init_state deck = 
  {
    player1 = {
      name = "Player 1";
      cards = []
    };
    player2 = {
      name = "Player 2";
      cards = []
    };
    deck = deck;
    discard_pile = [];
  }

let state_of_result result =
  match result with
  | Legal (x) -> x
  | Illegal -> empty_state

let distribute_cards deck =
  let step1 = (draw_deck (init_state deck) 
                 (init_state deck).player1) in 
  let step2 = (draw_deck (state_of_result step1) 
                 (state_of_result step1).player2) in 
  let step3 = (draw_deck (state_of_result step2) 
                 (state_of_result step2).player1) in 
  let step4 = (draw_deck (state_of_result step3) 
                 (state_of_result step3).player2) in 
  let step5 = (draw_deck (state_of_result step4) 
                 (state_of_result step4).player1) in 
  let step6 = (draw_deck (state_of_result step5) 
                 (state_of_result step5).player2) in 
  let step7 = (draw_deck (state_of_result step6) 
                 (state_of_result step6).player1) in 
  let step8 = (draw_deck (state_of_result step7) 
                 (state_of_result step7).player2) in 
  draw_deck (state_of_result step8) (state_of_result step8).player1

let current_hand st player =
  if st.player1 = player then st.player1.cards else st.player2.cards

let player1_hand st =
  st.player1.cards

let player1 st =
  st.player1

let player2_hand st =
  st.player2.cards

let player2 st =
  st.player2

let top_of_discard_pile st =
  List.hd st.discard_pile

let draw_discard st player =
  match st.discard_pile with
  | [] -> Illegal
  | h :: t -> if player.name = "Player 1" 
    then Legal ({
        player1 = {
          name = "Player 1";
          cards = h :: st.player1.cards
        };
        player2 = st.player2;
        deck = st.deck;
        discard_pile = t;
      })
    else Legal ({
        player1 = st.player1;
        player2 = {
          name = "Player 2";
          cards = h :: st.player2.cards
        };
        deck = st.deck;
        discard_pile = t;
      })

let rec remove_card card_list card =
  match card_list with 
  | [] -> []
  | h :: t -> if h = card then t else h :: remove_card t card

let discard st player card =
  if List.mem card player.cards
  then 
    if player.name = "Player 1" 
    then Legal ({
        player1 = {
          name = "Player 1";
          cards = remove_card player.cards card
        };
        player2 = st.player2;
        deck = st.deck;
        discard_pile = card :: st.discard_pile;
      })
    else Legal ({
        player1 = st.player1;
        player2 = {
          name = "Player 2";
          cards = remove_card player.cards card
        };
        deck = st.deck;
        discard_pile = card :: st.discard_pile;
      })

  else Illegal

let rec string_of_cards card_list =
  match card_list with
  | [] -> ""
  | h :: [] -> card_to_string h ^ "."
  | h :: t -> card_to_string h ^ ", " ^ string_of_cards t 

let string_of_hand st =
  match st with
  | {player1; player2; deck; discard_pile} ->
    string_of_cards player1.cards

let rec max_val lst =
  match lst with
  | [] -> 0
  | x :: [] -> x
  | x :: t -> max x (max_val t)

let score_of_hand player =
  max_val [
    Card.score_of_suit Spades player.cards;
    Card.score_of_suit Hearts player.cards;
    Card.score_of_suit Clubs player.cards;
    Card.score_of_suit Diamonds player.cards
  ]

(* let score player =
   List.fold_left (fun acc x -> acc + points x) 0 player.cards *)

let peek_discard st =
  match st.discard_pile with
  | [] -> "No cards in the discard pile."
  | x :: t -> card_to_string x

let peek_deck st =
  match st.deck with
  | [] -> "No cards in the discard pile."
  | x :: t -> card_to_string x


(* PLAYER 2 EXCLUSIVE FUNCTIONS *)
(* return the first card in player2's hand *)
let player2_discard_card st =
  List.hd st.player2.cards

(* returns the suit that the owner has the most of *)
let greatest_suit player =
  if score_of_hand player = Card.score_of_suit Spades player.cards 
  then Spades
  else if score_of_hand player = Card.score_of_suit Hearts player.cards 
  then Hearts
  else if score_of_hand player = Card.score_of_suit Clubs player.cards 
  then Clubs
  else Diamonds

(* returns the cards that the player should discard, i.e. the cards with a
   suit that the player doesn't have the most of *)
let cards_to_discard player =
  List.filter (fun c -> (suit_of_card c) != greatest_suit player) player.cards

(* decides where the player should draw from, e.g. the deck or the discard pile.
   the decision is made based on whether the suit of the card on top of the
   discard pile matches the one they are 'collecting', i.e. the one they have
   the most of. if the top of the discard pile has that suit, they draw from
   the discard pile, else they draw from the deck. *)
let player2_where_to_draw st : result =
  if st.discard_pile != [] && 
     greatest_suit st.player2 = suit_of_card (List.hd st.discard_pile)
  then draw_discard st st.player2
  else draw_deck st st.player2

(* decides what card the player should discard. there are 2 scenarios
   1. if the player has cards of the same suit, discard the minimum
   2. if the player has cards with different suit, discard the minimum
   that is not part of the suit they are 'collecting'. *)
let player2_what_to_discard st : card = 
  if cards_to_discard st.player2 != []
  then find_min_card (cards_to_discard st.player2) 
      (List.hd (cards_to_discard st.player2))
  else find_min_card (st.player2.cards) (List.hd st.player2.cards)

(* algorithm for player 2:
   1. identify suit of which the player has the largest number of cards
   2. decide whether to draw from. player will draw from the deck if the discard
   pile doesn't have the suit they're collecting.
   3. discard smallest card that isn't being collected *)
let player2_smart_move st = 
  let state_after_draw = player2_where_to_draw st |> state_of_result in
  discard state_after_draw state_after_draw.player2 
    (player2_what_to_discard state_after_draw)

let player2_discard_minimum st : card = 
  find_min_card (st.player2.cards) (List.hd st.player2.cards)

let player2_not_very_smart_move st =
  if (List.length st.deck = 0) then Illegal
  else let state_after_draw = state_of_result (draw_deck st st.player2) in
    discard state_after_draw state_after_draw.player2 
      (player2_discard_minimum state_after_draw)

let player2_move st =
  if (List.length st.deck = 0) then Illegal
  else let new_st = state_of_result (draw_deck st st.player2) in
    discard new_st new_st.player2 (player2_discard_card new_st)

let debug_string_of_hand st =
  match st with
  | {player1; player2; deck; discard_pile} ->
    string_of_cards player2.cards

let debug_player2_hand st =
  st.player2.cards
