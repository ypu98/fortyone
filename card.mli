(**
    Representation of static card data.

    This module represents the data stored in cards. This includes the 52 cards
    used in the card game, and each card has a suit, number, and points.

*)

(** The abstract type of values representing cards. *)
type card

(** The abstract type of values representing a deck of cards. *)
type deck

(** The type of values for a card. *)
type value = int

(** The rank of the card. *)
type rank = string

(** The suit of the card. *)
type suit = Spades | Hearts | Clubs | Diamonds

exception NoSuchCard

(** [points c] is the number of points of a card [c]. *)
val points : card -> int

(** [suit c] is the suit of a card [c]. *)
val suit : card -> suit

(** a deck of cards, in increasing order, organized by suit *)
val ordered_deck : card list

(** a sample deck of cards, randomly arranged *)
val deck1 : card list

(** [card to string card] parses a card into a string *)
val card_to_string : card -> string

(** [suit_to_string suit] parses a suit into a string *)
val suit_to_string : suit -> string

(** [card_of_cmd cmd_phrase] parses a command phrase and creates a card *)
val card_of_cmd : string list -> card

(** [get_val rank] gets the points associated with the value *)
val get_val : rank -> int

(** [score_of_suit suit cards] counts the total number of points of cards of the
    same suit in a list of cards. *)
val score_of_suit : suit -> card list -> int

(** [suit_count suit cards] counts the total number of cards of a given suit. *)
val suit_count : suit -> card list -> int

(** [suit_of_card card] is the suit of a card *)
val suit_of_card : card -> suit

(** [find_min_card cards c] finds the minimum card in a hand given a card [c]
    to compare it to. *)
val find_min_card : card list -> card -> card